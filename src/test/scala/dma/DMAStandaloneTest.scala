package ee290cdma

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule}
import verif._

class dmaSanityTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val p: Parameters = VerifTestUtils.getVerifParameters(xLen = 32) // Testing for our 32b RISC-V chip

  it should "elaborate the DMA <> TLXbar/TLRAM Standalone Block" in {
    val dut = LazyModule(new DMATLRAMStandaloneBlock(VerifTestUtils.getVerifTLMasterPortParameters(), VerifTestUtils.getVerifTLSlavePortParameters(),
      AddressSet(0x0, BigInt("1"*32, 2)), 256))
    test(dut.module) { _ =>
      assert(true)
    }
  }
}
