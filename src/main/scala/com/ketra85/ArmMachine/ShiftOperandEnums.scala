package com.ketra85.ArmMachine

object ShiftOperandEnums {
  sealed trait ShiftOperands
  case object LSL extends ShiftOperands
  case object LSR extends ShiftOperands
  case object ASR extends ShiftOperands
  case object ROR extends ShiftOperands
  case object RRX extends ShiftOperands

  val operands = Seq(LSL, LSR, ASR, ROR, RRX)
}
