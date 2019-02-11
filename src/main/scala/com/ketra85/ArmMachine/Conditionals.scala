package com.ketra85.ArmMachine

import com.ketra85.ArmMachine.Conditionals.Cond

// Remember Addressing modes for instructions
// B is weird check that
// which should represent arg3, think of a better representation

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