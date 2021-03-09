package ee290cdma

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import ee290cdma.DMAUtils._
import org.scalatest.flatspec.AnyFlatSpec
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}

import scala.util.Random
import verif._

class DMAStandaloneTest extends AnyFlatSpec with ChiselScalatestTester {
//  it should "elaborate the DMA <> TLXbar/TLRAM Standalone Block" in {
//    implicit val p: Parameters = VerifTestUtils.getVerifParameters(xLen = 32) // Testing for our 32b RISC-V chip
//    val dut = LazyModule(new DMATLRAMStandaloneBlock(VerifTestUtils.getVerifTLMasterPortParameters(), VerifTestUtils.getVerifTLSlavePortParameters(),
//      AddressSet(0x0, 0xffff), 8))
//    test(dut.module) { _ =>
//      assert(true)
//    }
//  }

  it should "sanity check basic write + read operations (max-read == beatBytes)" in {
    val beatBytes = 8
    val pAdderBits = 32
    implicit val p: Parameters = VerifTestUtils.getVerifParameters(xLen = 32, beatBytes = beatBytes, pAddrBits = pAdderBits)
    val dut = LazyModule(new DMAStandaloneBlock(VerifTestUtils.getVerifTLMasterPortParameters(),
      VerifTestUtils.getVerifTLSlavePortParameters(beatBytes = beatBytes, pAddrBits = pAdderBits),8))
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // Ensuring that parameters is properly propagated
      assert(beatBytes == dut.dma.writer.module.req.beatBytes)
      assert(pAdderBits == dut.dma.paddrBits)

      // VIP to send requests
      val writeDriver = new DecoupledDriverMaster[EE290CDMAWriterReq](c.clock, c.dma_in.write.req)
      val writeTxProto = new DecoupledTX(new EE290CDMAWriterReq(pAdderBits, beatBytes))
      val readDriver = new DecoupledDriverMaster[EE290CDMAReaderReq](c.clock, c.dma_in.read.req)
      val readTxProto = new DecoupledTX(new EE290CDMAReaderReq(pAdderBits, beatBytes))

      // VIP to read responses + data
      val readRespDriver = new DecoupledDriverSlave[EE290CDMAReaderResp](c.clock, c.dma_in.read.resp, 0)
      val respMonitor = new DecoupledMonitor[EE290CDMAReaderResp](c.clock, c.dma_in.read.resp)
      val readDataDriver = new DecoupledDriverSlave[UInt](c.clock, c.dma_in.read.queue, 0)
      val dataMonitor = new DecoupledMonitor[UInt](c.clock, c.dma_in.read.queue)

      // VIP to act as backing memory
      val slaveFn = new TLMemoryModel(dut.to_mem.params)
      val slaveModel = new TLDriverSlave(c.clock, dut.to_mem, slaveFn, TLMemoryModel.State.empty())
      val slaveMonitor = new TLMonitor(c.clock, dut.to_mem)

      val r = new Random

      c.clock.step(r.nextInt(10) + 10)

      // TODO Currently fails due to strange bug. See DMA.scala:188
      for (_ <- 0 until 20) {
        val writeSizeBytes = r.nextInt(beatBytes) + 1
        val req = GetRandomDMAWriterReq(pAdderBits, writeSizeBytes, beatBytes, r)
        writeDriver.push(writeTxProto.tx(req))
        c.clock.step(3) // Unsure why this is 3
        assert(c.dma_in.writeBusy.peek().litToBoolean)
        c.clock.step(50)
        assert(!c.dma_in.writeBusy.peek().litToBoolean)

        readDriver.push(readTxProto.tx(EE290CDMAReaderReqHelper(req.addr.litValue(), writeSizeBytes, pAdderBits, beatBytes)))
        c.clock.step(2)
        assert(c.dma_in.readBusy.peek().litToBoolean)
        c.clock.step(50)
        assert(!c.dma_in.readBusy.peek().litToBoolean)

//        // Debug printing
//        println(s"Read Driver queue: ${readDriver.inputTransactions.size}")
//        val txns = slaveMonitor.getMonitoredTransactions()
//        println(s"Montior txns: ${txns.size}")
//        txns.foreach(println(_))
//        println(s"Data: ${req.data.litValue()}")

        assert(respMonitor.monitoredTransactions.nonEmpty)
        assert(dataMonitor.monitoredTransactions.nonEmpty)
        assert(dataMonitor.monitoredTransactions.head.data.litValue() == req.data.litValue())
        respMonitor.clearMonitoredTransactions()
        dataMonitor.clearMonitoredTransactions()
      }
    }
  }

//  it should "sanity check max write + read operations" in {
//    val dut = LazyModule(new DMATLRAMStandaloneBlock(VerifTestUtils.getVerifTLMasterPortParameters(), VerifTestUtils.getVerifTLSlavePortParameters(),
//      AddressSet(0x0, 0xfffff), 256))
//    test(dut.module) { c =>
//
//    }
//  }

//  it should "sanity check min (0 bytes) write + read operations" in {
//    val dut = LazyModule(new DMATLRAMStandaloneBlock(VerifTestUtils.getVerifTLMasterPortParameters(), VerifTestUtils.getVerifTLSlavePortParameters(),
//      AddressSet(0x0, 0xfffff), 256))
//    test(dut.module) { c =>
//
//    }
//  }
}
