package ee290cdma

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.WithoutTLMonitors

// Note: Moved away from TLRAM due to address limitations, easier/efficient to use the TLMemoryModel to mock a 32-bit address space
class DMAStandaloneBlock(mPortParams: TLMasterPortParameters, sPortParams: TLSlavePortParameters, maxReadSize: Int)(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val bParams= TLBundleParameters(mPortParams, sPortParams)

  val dma  = LazyModule(new EE290CDMA(sPortParams.beatBytes, maxReadSize, "TLRAMDMA Test"))

  // IO to mimic memory interface
  val ioOutNode = BundleBridgeSink[TLBundle]()
  val to_mem = InModuleBody { ioOutNode.makeIO() }

//  // Test IO for debug/initializing TLRAM
//  val ioInNode = BundleBridgeSource(() => TLBundle(bParams))
//  val test_in = InModuleBody { ioInNode.makeIO() }

  ioOutNode := TLToBundleBridge(sPortParams) := dma.id_node
//  := BundleBridgeToTL(mPortParams) := ioInNode

  lazy val module = new LazyModuleImp(this) {
    val dma_in = IO(chiselTypeOf(dma.module.io))
    dma_in <> dma.module.io
  }
}
