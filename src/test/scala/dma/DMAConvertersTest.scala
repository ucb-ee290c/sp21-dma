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

  val inDoneReg = RegNext(io.in.done)
  val inDataQ = Module(new Queue(assembler.io.in.data.bits.cloneType, 1))
  val outQ = Module(new Queue(assembler.io.out.bits.cloneType, 1))

  inDataQ.io.enq <> io.in.data
  assembler.io.in.data <> inDataQ.io.deq
  assembler.io.in.done := inDoneReg
  outQ.io.enq <> assembler.io.out
  io.out <> outQ.io.deq
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

  it should "Test DMA packet builder with all full packets" in {
    val beatBytes = 4
    test(new DMAPacketAssemblerTestWrapper(beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriver = new DecoupledDriverMaster(c.clock, c.io.in.data)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.out, 0)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.out)

      val inVals = Seq.tabulate(20*beatBytes)(_ => scala.util.Random.nextInt(255))

      c.io.in.done.poke(false.B)

      inDriver.push(inVals.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))

      c.clock.step(inVals.length+200)

      outMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(seqToWidePackets(beatBytes, inVals))
        .foreach {case (o, e) => assert(o == e)}
    }
  }

  it should "Test DMA packet builder with a last unfilled packet" in {
    val beatBytes = 4
    for (i <- 1 until beatBytes) {
      test(new DMAPacketAssemblerTestWrapper(beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        val inDriver = new DecoupledDriverMaster(c.clock, c.io.in.data)
        val outDriver = new DecoupledDriverSlave(c.clock, c.io.out, 0)
        val outMonitor = new DecoupledMonitor(c.clock, c.io.out)


        val inVals = Seq.tabulate(2 * beatBytes + i)(_ => scala.util.Random.nextInt(255))

        fork.withRegion(TestdriverMain) {
          c.io.in.done.poke(false.B)
        }

        inDriver.push(inVals.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))

        c.clock.step(inVals.length + 200)

        fork.withRegion(TestdriverMain) {
          c.io.in.done.poke(true.B)
          c.clock.step()
          c.io.in.done.poke(false.B)
        }.join()

        c.clock.step(50)

        outMonitor.monitoredTransactions
          .map(x => x.data.litValue())
          .zip(seqToWidePackets(beatBytes, inVals))
          .foreach { case (o, e) => assert(o == e) }
      }
    }
  }
}