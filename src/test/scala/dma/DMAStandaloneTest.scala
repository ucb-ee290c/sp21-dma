package ee290cdma

import chiseltest._
import ee290cdma.DMAUtils._
import org.scalatest.flatspec.AnyFlatSpec
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import freechips.rocketchip.subsystem.SystemBusKey

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
    implicit val p: Parameters = VerifTestUtils.getVerifParameters(xLen = 32, beatBytes = 8)
    // NOTE: Only using 16bits for TLRAM testing (bugs out at 32)
    val dut = LazyModule(new DMATLRAMStandaloneBlock(VerifTestUtils.getVerifTLMasterPortParameters(), VerifTestUtils.getVerifTLSlavePortParameters(),
      AddressSet(0x0, 0xffff), 8))
    test(dut.module) { c =>
      val beatBytes = p(SystemBusKey).beatBytes
      val writeDriver = new DecoupledDriverMaster[EE290CDMAWriterReq](c.clock, dut.dma_in.write.req)
      val writeTxProto = new DecoupledTX(new EE290CDMAWriterReq(32, beatBytes)) // How to get addr bits from p?
      val readDriver = new DecoupledDriverMaster[EE290CDMAReaderReq](c.clock, dut.dma_in.read.req)
      val readTxProto = new DecoupledTX(new EE290CDMAReaderReq(32, beatBytes))
      val readRespDriver = new DecoupledDriverSlave[EE290CDMAReaderResp](c.clock, dut.dma_in.read.resp, 0)
      val monitor = new DecoupledMonitor[EE290CDMAReaderResp](c.clock, dut.dma_in.read.resp)

      val r = new Random

      c.clock.step(r.nextInt(100))

      var addr = r.nextInt(1 << 16)
      var writeSizeBytes = r.nextInt(beatBytes + 1)
      var data = r.nextInt(1 << (writeSizeBytes * 8))
      writeDriver.push(writeTxProto.tx(EE290CDMAWriterReqHelper(addr, data, writeSizeBytes, 32, beatBytes)))
      c.clock.step(r.nextInt(100))

      readDriver.push(readTxProto.tx(EE290CDMAReaderReqHelper(addr, writeSizeBytes, 32, beatBytes)))
      c.clock.step(r.nextInt(100))

      assert(monitor.monitoredTransactions.nonEmpty)
      assert(dut.dma_in.read.queue.deq().litValue() == data)
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
