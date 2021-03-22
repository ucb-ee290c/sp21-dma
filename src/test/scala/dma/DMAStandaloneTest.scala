package ee290cdma

import chisel3._
import chisel3.util.log2Ceil
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

  // This test writes random data and checks that later read results are consistent
  it should "sanity check basic write + read operations (max-read <= beatBytes)" in {
    val beatBytes = 4
    val pAdderBits = 32
    val maxReadBytes = 4
    implicit val p: Parameters = VerifTestUtils.getVerifParameters(xLen = 32, beatBytes = beatBytes, pAddrBits = pAdderBits)
    val dut = LazyModule(new DMAStandaloneBlock(VerifTestUtils.getVerifTLMasterPortParameters(),
      VerifTestUtils.getVerifTLSlavePortParameters(beatBytes = beatBytes, pAddrBits = pAdderBits),maxReadBytes))
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // Ensuring that parameters is properly propagated
      assert(beatBytes == dut.dma.writer.module.req.beatBytes)
      assert(pAdderBits == dut.dma.paddrBits)

      // VIP to send requests
      val writeDriver = new DecoupledDriverMaster[EE290CDMAWriterReq](c.clock, c.dma_in.write.req)
      val writeTxProto = new DecoupledTX(new EE290CDMAWriterReq(pAdderBits, beatBytes))
      val readDriver = new DecoupledDriverMaster[EE290CDMAReaderReq](c.clock, c.dma_in.read.req)
      val readTxProto = new DecoupledTX(new EE290CDMAReaderReq(pAdderBits, maxReadBytes))

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

      for (i <- 0 until 20) {
        val writeSize = r.nextInt(log2Ceil(beatBytes) + 1)
        val req = GetRandomDMAWriterReq(pAdderBits, writeSize, beatBytes, r)
        writeDriver.push(writeTxProto.tx(req))
        c.clock.step(3)
        assert(c.dma_in.writeBusy.peek().litToBoolean)
        c.clock.step(20)
        assert(!c.dma_in.writeBusy.peek().litToBoolean)

        readDriver.push(readTxProto.tx(EE290CDMAReaderReqHelper(req.addr.litValue(), 1 << writeSize, pAdderBits, maxReadBytes)))
        c.clock.step(2)
        assert(c.dma_in.readBusy.peek().litToBoolean)
        c.clock.step(20)
        assert(!c.dma_in.readBusy.peek().litToBoolean)

//        // Debug printing
//        println(s"Read Driver queue: ${readDriver.inputTransactions.size}")
//        val txns = slaveMonitor.getMonitoredTransactions()
//        println(s"Montior txns: ${txns.size}")
//        txns.foreach(println(_))
//        println(s"DataSize: $writeSize")
//        println(s"Data: ${req.data.litValue()}")

        assert(respMonitor.monitoredTransactions.nonEmpty)
        assert(dataMonitor.monitoredTransactions.nonEmpty)
        // Masking/shifting read data to extract written data
        var data = dataMonitor.monitoredTransactions.head.data.litValue()
        data = data >> ((req.addr.litValue() & (beatBytes - 1)) * 8).toInt
        data = data & BigInt("1" * ((1 << writeSize) * 8), 2)
        assert(data == req.data.litValue())
        respMonitor.clearMonitoredTransactions()
        dataMonitor.clearMonitoredTransactions()

//        // Debug printing
//        println(s"Passed $i, ${req.data.litValue()}")
      }
    }
  }

  // This test writes random data and checks that later read results are consistent (multiple packets)
  it should "sanity check basic write + read operations (max-read > beatBytes)" in {
    val beatBytes = 4
    val pAdderBits = 32
    val maxReadBytes = 8
    implicit val p: Parameters = VerifTestUtils.getVerifParameters(xLen = 32, beatBytes = beatBytes, pAddrBits = pAdderBits)
    val dut = LazyModule(new DMAStandaloneBlock(VerifTestUtils.getVerifTLMasterPortParameters(),
      VerifTestUtils.getVerifTLSlavePortParameters(beatBytes = beatBytes, pAddrBits = pAdderBits),maxReadBytes))
    test(dut.module).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // Ensuring that parameters is properly propagated
      assert(beatBytes == dut.dma.writer.module.req.beatBytes)
      assert(pAdderBits == dut.dma.paddrBits)

      // VIP to send requests
      val writeDriver = new DecoupledDriverMaster[EE290CDMAWriterReq](c.clock, c.dma_in.write.req)
      val writeTxProto = new DecoupledTX(new EE290CDMAWriterReq(pAdderBits, beatBytes))
      val readDriver = new DecoupledDriverMaster[EE290CDMAReaderReq](c.clock, c.dma_in.read.req)
      val readTxProto = new DecoupledTX(new EE290CDMAReaderReq(pAdderBits, maxReadBytes))

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

      for (i <- 0 until 20) {
        val writeSize = r.nextInt(log2Ceil(beatBytes) + 1)
        val req = GetRandomDMAWriterReq(pAdderBits, writeSize, beatBytes, r)
        writeDriver.push(writeTxProto.tx(req))
        c.clock.step(3)
        assert(c.dma_in.writeBusy.peek().litToBoolean)
        c.clock.step(20)
        assert(!c.dma_in.writeBusy.peek().litToBoolean)

        val readSizeBytes = r.nextInt(maxReadBytes) + 1
        readDriver.push(readTxProto.tx(EE290CDMAReaderReqHelper(req.addr.litValue(), readSizeBytes, pAdderBits, maxReadBytes)))
        c.clock.step(2)
        assert(c.dma_in.readBusy.peek().litToBoolean)
        c.clock.step(20)
        assert(!c.dma_in.readBusy.peek().litToBoolean)

        //        // Debug printing
        //        println(s"Read Driver queue: ${readDriver.inputTransactions.size}")
        //        val txns = slaveMonitor.getMonitoredTransactions()
        //        println(s"Montior txns: ${txns.size}")
        //        txns.foreach(println(_))
        //        println(s"DataSize: $writeSize")
        //        println(s"Data: ${req.data.litValue()}")

        assert(respMonitor.monitoredTransactions.nonEmpty)
        assert(dataMonitor.monitoredTransactions.nonEmpty)
        // Masking/shifting read data to extract written data
        var data = dataMonitor.monitoredTransactions.head.data.litValue()
        data = data >> ((req.addr.litValue() & (beatBytes - 1)) * 8).toInt
        data = data & BigInt("1" * ((1 << writeSize) * 8), 2)
        assert(data == req.data.litValue())
        if (readSizeBytes > beatBytes) {
          // If readSizeBytes > beatBytes, need to check second beat of data
          assert(dataMonitor.monitoredTransactions.size == 2)
          // Extracting second beat of data directly from the memory state
          val bytesPerWord = slaveModel.params.dataBits/8
          val addr = ((req.addr.litValue() / bytesPerWord) + 1).toLong
          assert(dataMonitor.monitoredTransactions(1).data.litValue() == TLMemoryModel.read(slaveModel.state.mem, addr, bytesPerWord, -1))
        }
        respMonitor.clearMonitoredTransactions()
        dataMonitor.clearMonitoredTransactions()

        //        // Debug printing
        //        println(s"Passed $i, ${req.data.litValue()}")
      }
    }
  }
}
