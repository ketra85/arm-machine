package com.ketra85.ArmMachine

import com.ketra85.ArmMachine.Conditionals.Cond

// Remember Addressing modes for instructions
// B is weird check that
// which should represent arg3, think of a better representation
class Instruction(name: String, conditional: Cond, extraBit: Int,
                  arg1: String, arg2: String, arg3: String,
                  arg4: String, arg5: String) {

  def this(name: String, arg1: String) {
    this(name, Conditionals.NULL, 0, arg1, "", "", "", "")
  }

  def this(name: String, Conditional: Cond, arg1: String) {
    this(name, Conditional, 0, arg1, "", "", "", "")
  }

  def this(name: String, arg1: String, arg2: String) {
    this(name, Conditionals.NULL, 0, arg1, arg2, "", "", "")
  }

  def this(name: String, arg1: String, arg2: String, arg3: String) {
    this(name, Conditionals.NULL, 0, arg1, arg2, arg3, "", "")
  }

  def this(name: String, extraBit: Int, arg1: String, arg2: String, arg3: String) {
    this(name, Conditionals.NULL, extraBit, arg1, arg2, arg3, "", "")
  }

  def this(name: String, conditional: Cond, arg1: String, arg2: String, arg3: String, arg4: String) {
    this(name, conditional, 0, arg1, arg2, arg3, arg4, "")
  }

  def this(name: String, conditional: Cond, arg1: String, arg2: String, arg3: String, arg5: String) {
    this(name, conditional, 0, arg1, arg2, arg3, arg4, arg5)
  }


}


// Conditionals on which instructions can instantiated as
// Ex: MOVEQ
// Most ARM instruction can accomodate a conditional in its call
object Conditionals {
  sealed trait Cond
  case object EQ extends Cond
  case object NE extends Cond
  case object CS extends Cond
  case object HS extends Cond
  case object CC extends Cond
  case object LO extends Cond
  case object MI extends Cond
  case object PL extends Cond
  case object VS extends Cond
  case object VC extends Cond
  case object HI extends Cond
  case object LS extends Cond
  case object GE extends Cond
  case object LT extends Cond
  case object GT extends Cond
  case object LE extends Cond
  case object AL extends Cond
  case object NULL extends Cond

  val Conditionals = Seq(EQ, NE, CS, HS, CC, LO, MI,
    PL, VS, VC, HI, LS, GE, LT, GT, LE, AL, NULL)
}