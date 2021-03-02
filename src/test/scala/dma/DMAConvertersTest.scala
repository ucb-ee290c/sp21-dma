package ee290cdma

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import verif._

class DMAPacketAssemblerTestWrapper(beatBytes: Int) extends Module {
  val assembler = Module(new DMAPacketAssembler(beatBytes))

  val io = IO(assembler.io.cloneType)

  val producerDoneReg = RegNext(io.producer.done)
  val producerDoneQ = Module(new Queue(assembler.io.producer.data.bits.cloneType, 1))
  val dmaOutQ = Module(new Queue(assembler.io.dmaOut.bits.cloneType, 1))

  producerDoneQ.io.enq <> io.producer.data
  assembler.io.producer.data <> producerDoneQ.io.deq
  assembler.io.producer.done := producerDoneReg
  dmaOutQ.io.enq <> assembler.io.dmaOut
  io.dmaOut <> dmaOutQ.io.deq
}

class DMAPacketDisassemblerTestWrapper(beatBytes: Int) extends Module {
  val disassembler = Module(new DMAPacketDisassembler(beatBytes))

  val io = IO(disassembler.io.cloneType)

  val consumerDoneReg = RegNext(io.consumer.done)
  val dmaInQ = Module(new Queue(disassembler.io.dmaIn.bits.cloneType, 1))
  val consumerDataQ = Module(new Queue(disassembler.io.consumer.data.bits.cloneType, 1))

  dmaInQ.io.enq <> io.dmaIn
  disassembler.io.dmaIn <> dmaInQ.io.deq
  disassembler.io.consumer.done := consumerDoneReg
  consumerDataQ.io.enq <> disassembler.io.consumer.data
  io.consumer.data <> consumerDataQ.io.deq
}

class DMAConvertersTest extends AnyFlatSpec with ChiselScalatestTester {
  def seqToWidePackets(beatBytes: Int, seq: Seq[Int]): Seq[BigInt] = {
    var in = seq
    var out = Seq[BigInt]()
    while (in.nonEmpty) {
      val (group, rest) = in.splitAt(beatBytes)
      val bytes = group.padTo(beatBytes, 0)

      var sum = BigInt(0)
      for (i <- 0 until beatBytes) {
        sum = sum + (BigInt(bytes(i)) << (8*i))
      }
      out = out :+ sum
      in = rest
    }
    out
  }

  it should "Test DMA packet builder when producer produces a multiple of beatBytes" in {
    val beatBytes = 4
    test(new DMAPacketAssemblerTestWrapper(beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriver = new DecoupledDriverMaster(c.clock, c.io.producer.data)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.dmaOut, 0)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.dmaOut)

      val inVals = Seq.tabulate(20*beatBytes)(_ => scala.util.Random.nextInt(255))

      c.io.producer.done.poke(false.B)

      inDriver.push(inVals.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))

      c.clock.step(inVals.length+200)

      outMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(seqToWidePackets(beatBytes, inVals))
        .foreach {case (o, e) => assert(o == e)}
    }
  }

  it should "Test DMA packet builder when producer leaves unfilled packet and signals done" in {
    val beatBytes = 4
    for (i <- 1 until beatBytes) {
      test(new DMAPacketAssemblerTestWrapper(beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        val inDriver = new DecoupledDriverMaster(c.clock, c.io.producer.data)
        val outDriver = new DecoupledDriverSlave(c.clock, c.io.dmaOut, 0)
        val outMonitor = new DecoupledMonitor(c.clock, c.io.dmaOut)


        val inVals = Seq.tabulate(2 * beatBytes + i)(_ => scala.util.Random.nextInt(255))

        fork.withRegion(TestdriverMain) {
          c.io.producer.done.poke(false.B)
        }

        inDriver.push(inVals.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))

        c.clock.step(inVals.length + 200)

        fork.withRegion(TestdriverMain) {
          c.io.producer.done.poke(true.B)
          c.clock.step()
          c.io.producer.done.poke(false.B)
        }.join()

        c.clock.step(50)

        outMonitor.monitoredTransactions
          .map(x => x.data.litValue())
          .zip(seqToWidePackets(beatBytes, inVals))
          .foreach { case (o, e) => assert(o == e) }
      }
    }
  }

  it should "Test DMA packet disassembler when consumer requests all packets" in {
    val beatBytes = 4
    test(new DMAPacketDisassemblerTestWrapper(beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriver = new DecoupledDriverMaster(c.clock, c.io.dmaIn)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.consumer.data, 0)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.consumer.data)

      val outVals = Seq.tabulate(20*beatBytes)(_ => scala.util.Random.nextInt(255))
      val inVals = seqToWidePackets(beatBytes, outVals)

      c.io.consumer.done.poke(false.B)

      inDriver.push(inVals.map(x => (new DecoupledTX(UInt(beatBytes.W))).tx(x.U)))

      c.clock.step(inVals.length+200)

      outMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(outVals)
        .foreach {case (o, e) => assert(o == e)}
    }
  }

  it should "Test DMA packet disassembler when consumer asserts done" in {
    val beatBytes = 4
    test(new DMAPacketDisassemblerTestWrapper(beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriver = new DecoupledDriverMaster(c.clock, c.io.dmaIn)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.consumer.data, 0)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.consumer.data)

      val outVals = Seq.tabulate(20*beatBytes)(_ => scala.util.Random.nextInt(254) + 1) // Assure none can be zero for test
      val inVals = seqToWidePackets(beatBytes, outVals)

      c.io.consumer.done.poke(false.B)

      inDriver.push(inVals.map(x => (new DecoupledTX(UInt(beatBytes.W))).tx(x.U)))

      c.clock.step(13)

      fork.withRegion(TestdriverMain) {
        c.io.consumer.done.poke(true.B)
      }.join()

      c.clock.step(inVals.length+200)

      var seenZero = false

      outMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(outVals)
        .foreach {
          case (o, e) => if (seenZero) {
            assert(o == 0)
          } else if (o == 0) {
            seenZero = true
          } else {
            assert(o == e)
          }
        }
    }
  }
}