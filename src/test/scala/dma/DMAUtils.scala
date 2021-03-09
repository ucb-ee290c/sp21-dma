package ee290cdma

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import scala.util.Random

object DMAUtils {
  def EE290CDMAWriterReqHelper(addr: BigInt, data: BigInt, totalBytes: Int, addrBits: Int, beatBytes: Int): EE290CDMAWriterReq = {
    new EE290CDMAWriterReq(addrBits, beatBytes).Lit(_.addr -> addr.U, _.data -> data.U, _.totalBytes -> totalBytes.U)
  }
  def EE290CDMAReaderReqHelper(addr: BigInt, totalBytes: Int, addrBits: Int, beatBytes: Int): EE290CDMAReaderReq = {
    new EE290CDMAReaderReq(addrBits, beatBytes).Lit(_.addr -> addr.U, _.totalBytes -> totalBytes.U)
  }
  def GetRandomDMAWriterReq(addrBits: Int, writeSizeBytes: Int, beatBytes: Int, r: Random): EE290CDMAWriterReq = {
    val addr = BigInt(addrBits, r) & ~BigInt("1" * writeSizeBytes * 8, 2)
    val data = BigInt(writeSizeBytes * 8, r)
    EE290CDMAWriterReqHelper(addr, data, writeSizeBytes, addrBits, beatBytes)
  }
}