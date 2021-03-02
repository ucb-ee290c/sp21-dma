package ee290cdma

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.tile.{HasCoreParameters}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}
import testchipip.TLHelper

class EE290CDMAWriterReq(addrBits: Int, beatBytes: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = UInt((beatBytes * 8).W)
  val totalBytes = UInt(log2Ceil(beatBytes).W)
}

class EE290CDMAReaderReq(addrBits: Int, maxReadSize: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val totalBytes = UInt(log2Ceil(maxReadSize).W)
}

class EE290CDMAReaderResp(maxReadSize: Int) extends Bundle {
  val bytesRead = UInt(log2Ceil(maxReadSize).W)
}

/*
Builds beatByte wide data packets for the DMA from the one-byte wide packets
 */
class DMAPacketAssembler(beatBytes: Int) extends Module {
  val io = IO(new Bundle {
    val in = new Bundle {
      val data = Flipped(Decoupled(UInt(8.W)))
      val done = Input(Bool()) // Signal to indicate we should send what we have and reset
    }
    val out = Decoupled(UInt((beatBytes*8).W))
  })

  val counter = RegInit(0.U(log2Ceil(beatBytes + 1).W))
  val packedData = RegInit(0.U((8 * beatBytes).W))

  when (io.in.data.fire()) {
    packedData := packedData | (io.in.data.bits << (counter << 3).asUInt()).asUInt()
    counter := counter + 1.U
  }

  when (io.out.fire()) {
    packedData := 0.U
    counter := 0.U
  }

  io.out.valid := counter === beatBytes.U | (counter =/= 0.U & io.in.done)
  io.out.bits := packedData
  // If we are waiting on the out to be taken up, we should not take in more data
  io.in.data.ready := !io.out.valid
}

/*
Builds one-byte wide data packets from the beatByte wide packets produced by the DMA
 */
class DMAPacketDisassembler(beatBytes: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(beatBytes.W)))
    val out = new Bundle {
      val data = Decoupled(UInt(8.W))
      val done = Input(Bool())
    }
  })

  val counter = RegInit(0.U(log2Ceil(beatBytes + 1).W))
  val wideData = RegInit(0.U((8 * beatBytes).W))

  when (io.in.fire()) {
    wideData := io.in.bits
    counter := beatBytes.U
  }

  when (io.out.data.fire()) {
    wideData := wideData >> 3
    counter := counter - 1.U
  }

  when (io.out.done) { // The assembler is done and we should reset to initial state
    wideData := 0.U
    counter := 0.U
  }

  io.in.ready := counter === 0.U
  io.out.data.valid := counter =/= 0.U
  io.out.data.bits := wideData(7, 0) // TODO: Verify this gives the next byte in order (endianness)
}

class EE290CDMAWriteIO(addrBits: Int, beatBytes: Int) extends Bundle {
  val req = Decoupled(new EE290CDMAWriterReq(addrBits, beatBytes))
}

class EE290CDMAReadIO(addrBits: Int, beatBytes: Int, maxReadSize: Int) extends Bundle {
  val req = Flipped(Decoupled(new EE290CDMAReaderReq(addrBits, maxReadSize)))
  val resp = Decoupled(new EE290CDMAReaderResp(maxReadSize))
  val queue = Decoupled(UInt((beatBytes * 8).W))
}


class EE290CDMA(beatBytes: Int, maxReadSize: Int, name: String)(implicit p: Parameters) extends LazyModule {
  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  val reader = LazyModule(new EE290CDMAReader(beatBytes, maxReadSize, s"${name}-reader"))
  val writer = LazyModule(new EE290CDMAWriter(beatBytes, s"${name}-writer"))

  xbar_node := writer.node
  xbar_node := reader.node
  id_node := xbar_node

  lazy val module = new LazyModuleImp(this) with HasCoreParameters {
    val io = IO(new Bundle {
      val read = new EE290CDMAReadIO(paddrBits, beatBytes, maxReadSize)
      val write = new EE290CDMAWriteIO(paddrBits, beatBytes)
      val busy = Output(Bool())
    })

    val readQ = Queue(reader.module.io.queue) // Queue of read data
    val writeQ = Queue(io.write.req) // Queue of write requests

    io.read.queue <> readQ

    reader.module.io.req <> io.read.req
    reader.module.io.resp <> io.read.resp

    writer.module.io.req <> writeQ

    io.busy := writer.module.io.busy | reader.module.io.busy
  }

}

class EE290CDMAWriter(beatBytes: Int, name: String)(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = name,
    sourceId = IdRange(0, 1)
  )

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (mem, edge) = node.out(0)

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new EE290CDMAWriterReq(paddrBits, beatBytes)))
      val busy = Bool()
    })

    val req = Reg(new EE290CDMAWriterReq(paddrBits, beatBytes))

    val s_idle :: s_write :: s_resp :: s_done :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val mask = VecInit(Seq.tabulate(beatBytes)(i => ((1 << i) - 1).U ))

    val bytesSent = Reg(UInt(log2Ceil(beatBytes).W))
    val bytesLeft = req.totalBytes - bytesSent

    val put = edge.Put(
      fromSource = 0.U, // TODO: Verify
      toAddress = req.addr,
      lgSize = log2Ceil(beatBytes).U,
      data = req.data)._2

    val putPartial = edge.Put(
      fromSource = 0.U,
      toAddress = req.addr,
      lgSize = log2Ceil(beatBytes).U,
      data = req.data,
      mask = mask(bytesLeft))._2

    mem.a.valid := state === s_write
    mem.a.bits := Mux(bytesLeft < beatBytes.U, put, putPartial)

    mem.d.ready := state === s_resp

    when (edge.done(mem.a)) {
      req.addr := req.addr + beatBytes.U
      bytesSent := bytesSent + Mux(bytesLeft < beatBytes.U, bytesLeft, beatBytes.U)
      state := s_resp
    }

    when (mem.d.fire()) {
      state := Mux(bytesLeft === 0.U, s_done, s_write)
    }

    io.req.ready := state === s_idle | state === s_done
    io.busy := ~io.req.ready

    when (io.req.fire()) {
      req := io.req.bits
      bytesSent := 0.U
      state := s_write
    }
  }
}

class EE290CDMAReader(beatBytes: Int, maxReadSize: Int, name: String)(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = name,
    sourceId = IdRange(0, 1)
  )

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (mem, edge) = node.out(0)

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new EE290CDMAReaderReq(paddrBits, maxReadSize)))
      val resp = Decoupled(new EE290CDMAReaderResp(maxReadSize))
      val queue = Decoupled(UInt((beatBytes * 8).W))
      val busy = Bool()
    })

    val req = Reg(new EE290CDMAReaderReq(paddrBits, maxReadSize))

    val s_idle :: s_read :: s_resp :: s_queue :: s_done :: Nil = Enum(5)
    val state = RegInit(s_idle)

    val bytesRead = Reg(UInt(log2Ceil(maxReadSize).W))
    val bytesLeft = req.totalBytes - bytesRead


    mem.a.bits := edge.Get(
      fromSource = 0.U, // TODO: see writer source comment
      toAddress = req.addr,
      lgSize = log2Ceil(beatBytes).U)._2 // Always get a full beatBytes bytes, even if not used in packet

    when (edge.done(mem.a)) {
      req.addr := req.addr + beatBytes.U
      bytesRead := bytesRead + Mux(bytesLeft < beatBytes.U, bytesLeft, beatBytes.U) // TODO: move down to mem.d.fire clause to allow for masking (?)
      state := s_resp
    }

    when (mem.d.fire()) {
      io.queue.bits := mem.d.bits.data // TODO: mask off the unwanted bytes if bytesLeft < beatBytes.U using a mask vector and register
      state := s_queue
    }

    when (io.queue.fire()) {
      state := Mux(bytesLeft === 0.U, s_done, s_read)
    }

    io.req.ready := state === s_idle | state === s_done
    io.resp.valid := state === s_done
    io.resp.bits.bytesRead := bytesRead
    io.queue.valid := state === s_queue
    io.busy := ~io.req.ready

    when (io.req.fire()) {
      req := io.req.bits
      state := s_read
    }
  }
}
