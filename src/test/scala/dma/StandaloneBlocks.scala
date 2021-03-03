package ee290cdma

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.WithoutTLMonitors

class DMATLRAMStandaloneBlock(mPortParams: TLMasterPortParameters, sPortParams: TLSlavePortParameters,
                              ramAddressSet: AddressSet, maxReadSize: Int)(implicit p: Parameters = new WithoutTLMonitors) extends LazyModule {
  val bParams= TLBundleParameters(mPortParams, sPortParams)

  val ram  = LazyModule(new TLRAM(ramAddressSet, cacheable = false, atomics = false, beatBytes = sPortParams.beatBytes))
  val xbar = LazyModule(new TLXbar)
  val dma  = LazyModule(new EE290CDMA(sPortParams.beatBytes, maxReadSize, "TLRAMDMA Test"))

  // Test IO for debug/initializing TLRAM
  val ioInNode = BundleBridgeSource(() => TLBundle(bParams))
  val test_in = InModuleBody { ioInNode.makeIO() }

  // IO for DMA
  val dma_in = InModuleBody{ dma.module.io }

  ram.node := TLBuffer() := xbar.node
  xbar.node := dma.id_node
  xbar.node := BundleBridgeToTL(mPortParams) := ioInNode

  lazy val module = new LazyModuleImp(this) {}
}
