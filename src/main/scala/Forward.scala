package Ov1

import chisel3._

class RegisterForwardSignals extends Bundle {
  val operand = UInt(5.W)
  val value = UInt(32.W)

  def valid() = operand =/= 0.U
}
