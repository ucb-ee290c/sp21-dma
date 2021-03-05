package ee290cdma

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

object DMAUtils {
  def EE290CDMAWriterReqHelper(addr: Int, data: Int, totalBytes: Int, addrBits: Int, beatBytes: Int): EE290CDMAWriterReq = {
    new EE290CDMAWriterReq(addrBits, beatBytes).Lit(_.addr -> addr.U, _.data -> data.U, _.totalBytes -> totalBytes.U)
  }
  def EE290CDMAReaderReqHelper(addr: Int, totalBytes: Int, addrBits: Int, beatBytes: Int): EE290CDMAReaderReq = {
    new EE290CDMAReaderReq(addrBits, beatBytes).Lit(_.addr -> addr.U, _.totalBytes -> totalBytes.U)
  }
}