package ee290cdma

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket.constants.{MemoryOpConstants}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}
import testchipip.TLHelper

class EE290CDMAWriterReq(val addrBits: Int, val beatBytes: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = UInt((beatBytes * 8).W)
  val totalBytes = UInt((log2Ceil(beatBytes+1)).W)
}

class EE290CDMAReaderReq(val addrBits: Int, val maxReadSize: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val totalBytes = UInt((log2Ceil(maxReadSize+1)).W)
}

class EE290CDMAReaderResp(val maxReadSize: Int) extends Bundle {
  val bytesRead = UInt((log2Ceil(maxReadSize+1)).W)
}

/*
Builds beatByte wide data packets for the DMA from the one-byte wide packets
 */
class DMAPacketAssembler(beatBytes: Int) extends Module {
  val io = IO(new Bundle {
    val producer = new Bundle {
      val data = Flipped(Decoupled(UInt(8.W)))
      val done = Input(Bool()) // Signal to indicate we should send what we have and reset
    }
    val dmaOut = Decoupled(UInt((beatBytes*8).W))
  })

  val counter = RegInit(0.U(log2Ceil(beatBytes + 1).W))
  val packedData = RegInit(0.U((8 * beatBytes).W))

  when (io.producer.data.fire()) {
    packedData := packedData | (io.producer.data.bits << (counter << 3).asUInt()).asUInt()
    counter := counter + 1.U
  }

  when (io.dmaOut.fire()) {
    packedData := 0.U
    counter := 0.U
  }

  io.dmaOut.valid := counter === beatBytes.U | (counter =/= 0.U & io.producer.done)
  io.dmaOut.bits := packedData
  // If we are waiting on the out to be taken up, we should not take in more data
  io.producer.data.ready := !io.dmaOut.valid
}

/*
Builds one-byte wide data packets from the beatByte wide packets produced by the DMA
 */
class DMAPacketDisassembler(beatBytes: Int) extends Module {
  val io = IO(new Bundle {
    val dmaIn = Flipped(Decoupled(UInt((beatBytes*8).W)))
    val consumer = new Bundle {
      val data = Decoupled(UInt(8.W))
      val done = Input(Bool())
    }
  })

  val counter = RegInit(0.U(log2Ceil(beatBytes + 1).W))
  val wideData = RegInit(0.U((8 * beatBytes).W))

  when (io.dmaIn.fire()) {
    wideData := io.dmaIn.bits
    counter := beatBytes.U
  }

  when (io.consumer.data.fire()) {
    wideData := wideData >> 8
    counter := counter - 1.U
  }

  when (io.consumer.done) { // The assembler is done and we should reset to initial state
    wideData := 0.U
    counter := 0.U
  }

  io.dmaIn.ready := counter === 0.U & io.consumer.data.ready
  io.consumer.data.valid := counter =/= 0.U
  io.consumer.data.bits := wideData(7, 0)
}

class EE290CDMAWriteIO(addrBits: Int, beatBytes: Int) extends Bundle {
  val req = Flipped(Decoupled(new EE290CDMAWriterReq(addrBits, beatBytes)))
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

  val paddrBits = 32 //TODO: is there an elegant way to get paddrBits into a non Tile based component

  xbar_node := writer.node
  xbar_node := reader.node
  id_node := xbar_node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val read = new EE290CDMAReadIO(paddrBits, beatBytes, maxReadSize)
      val write = new EE290CDMAWriteIO(paddrBits, beatBytes)
      val readBusy = Output(Bool())
      val writeBusy = Output(Bool())
    })

    val readQ = Queue(reader.module.io.queue) // Queue of read data
    val writeQ = Queue(io.write.req) // Queue of write requests

    io.read.queue <> readQ

    reader.module.io.req <> io.read.req
    reader.module.io.resp <> io.read.resp

    writer.module.io.req <> writeQ

    io.readBusy := reader.module.io.busy
    io.writeBusy := writer.module.io.busy
  }

}

class EE290CDMAWriter(beatBytes: Int, name: String)(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = name,
    sourceId = IdRange(0, 1) // Identifies the valid IDs for this node. NOTE: Does not influence actual bundle creation (e.g. it's just a label)
  )

  lazy val module = new LazyModuleImp(this) with MemoryOpConstants {
    val (mem, edge) = node.out(0)

    val paddrBits = edge.bundle.addressBits

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new EE290CDMAWriterReq(paddrBits, beatBytes)))
      val busy = Output(Bool())
    })

    val req = Reg(new EE290CDMAWriterReq(paddrBits, beatBytes))

    val s_idle :: s_write :: s_resp :: s_done :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val mask = VecInit(Seq.tabulate(beatBytes)(i => ((1 << i) - 1).U ))

    val bytesSent = Reg(UInt(log2Ceil(beatBytes).W))
    val bytesLeft = req.totalBytes - bytesSent

    val put = edge.Put(
      fromSource = 0.U, // TODO: Hardcoded to 0 for now, but will want to parameterize
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
    mem.a.bits := Mux(bytesLeft < beatBytes.U, putPartial, put)

//    mem.d.ready := state === s_resp
    // TODO Both writer and reader needs to have mem.d.ready high for the xbar.d.ready to be high for some reason...
    mem.d.ready := true.B

    // TODO Sometimes never occurs and causes infinite amount of requests... need to look into reason
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
    sourceId = IdRange(1, 2) // Identifies the valid IDs for this node. NOTE: Does not influence actual bundle creation (e.g. it's just a label)
  )

  lazy val module = new LazyModuleImp(this) with MemoryOpConstants {
    val (mem, edge) = node.out(0)

    val paddrBits = edge.bundle.addressBits

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new EE290CDMAReaderReq(paddrBits, maxReadSize)))
      val resp = Decoupled(new EE290CDMAReaderResp(maxReadSize))
      val queue = Decoupled(UInt((beatBytes * 8).W))
      val busy = Output(Bool())
    })

    val req = Reg(new EE290CDMAReaderReq(paddrBits, maxReadSize))

    val s_idle :: s_read :: s_resp :: s_queue :: s_done :: Nil = Enum(5)
    val state = RegInit(s_idle)

    val bytesRead = Reg(UInt(log2Ceil(maxReadSize).W))
    val bytesLeft = req.totalBytes - bytesRead

    val readBytes = Reg(UInt((beatBytes * 8).W))

    mem.a.valid := state === s_read
    mem.a.bits := edge.Get(
      fromSource = 1.U, // TODO: Hardcoded to not conflict with writer, but should parameterize (as will be connected to bus)
      toAddress = req.addr,
      lgSize = log2Ceil(beatBytes).U)._2 // Always get a full beatBytes bytes, even if not used in packet

    when (edge.done(mem.a)) {
      req.addr := req.addr + beatBytes.U
      bytesRead := bytesRead + Mux(bytesLeft < beatBytes.U, bytesLeft, beatBytes.U) // TODO: move down to mem.d.fire clause to allow for masking (?)
      state := s_resp
    }

//    mem.d.ready := state === s_resp
    // TODO Both writer and reader needs to have mem.d.ready high for the xbar.d.ready to be high for some reason...
    mem.d.ready := true.B

    when (mem.d.fire()) {
      readBytes := mem.d.bits.data // TODO: mask off the unwanted bytes if bytesLeft < beatBytes.U using a mask vector and register
      state := s_queue
    }

    when (io.queue.fire()) {
      state := Mux(bytesLeft === 0.U, s_done, s_read)
    }

    io.req.ready := state === s_idle | state === s_done
    io.resp.valid := state === s_done
    io.resp.bits.bytesRead := bytesRead
    io.queue.valid := state === s_queue
    io.queue.bits := readBytes
    io.busy := ~io.req.ready

    when (io.req.fire()) {
      req := io.req.bits
      state := s_read
    }
  }
}
