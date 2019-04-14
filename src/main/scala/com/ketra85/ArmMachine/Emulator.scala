package com.ketra85.ArmMachine

// Emulator class encapsulating emulator operations
// Memory, Registers, etc
class Emulator(memorySize: Int, numberOfRegisters: Int) {
//  private val DEFAULT_NUMBER_OF_GENERAL_REGISTERS = 15
//  private val DEFAULT_PC_REGISTERS = 1
//  private val DEFAULT_FLAG_REGISTERS = 2

  // A general, program counter, and flag registers
//  private val MIN_REGISTERS = 3

  // rewrite this
  // a gp, pc, and flag register is always needed
  // r15 pc
  // r14 link register
  // SPSR which is a copy of the flag register (CPSR)
//  val registers = numberOfRegisters match {
//    case x if x <= 3 =>
//      for(x <- DEFAULT_NUMBER_OF_GENERAL_REGISTERS)
//        yield new Register("r" + x, RegTypeEnums.generalUnbanked)
//      for(x <- DEFAULT_PC_REGISTERS)
//        yield new Register("r" + x, RegTypeEnums.pc)
//      for(x <- DEFAULT_FLAG_REGISTERS)
//        yield new Register("r" + x, RegTypeEnums.flag)
//    case _ => numberOfRegisters
//  }

  // reg 13 -> Stack Pointer
  // reg 14 -> Link Register
  // reg 15 -> Program Counter
  // BANKED AND UNBANKED REGS
  val registers = Array.fill[Int](16)(0)
  val memory = Array.fill[Int](65536)(0)

  //remember multiple SPSR
  val cpsr = Map("n" -> 0, "z" -> 0, "c" -> 0, "v" -> 0, "q" -> 0,
    "e" -> 0, "a" -> 0, "i" -> 0, "f" -> 0, "t" -> 0, "m" -> 0)

  val ASR = 2
  val LSL = 0
  val LSR = 1
  val ROR = 4
  val RRX = 3

  var shift_type = 0
  var shift_amount = 0
  var carry_out : Int = 0
  var overflow : Int = 0

  def getProgramCounter() : Int = {
    return registers(15) + 8
  }

  def getRegisters(i : Int) : Int = {
    if(i == 15) {
      return getProgramCounter()
    } else {
      return registers(i)
    }
  }

  def setPSR(value : Int, set_overflow: Boolean): Unit = {
    cpsr("n") = value >>> 31
    cpsr("z") = if (value == 0) 1 else 0
    cpsr("c") = carry_out

    if(set_overflow) {
      cpsr("v") = overflow
    }
  }

  def decodeImmShift(shiftType : Int, immediate5 : Int): Unit = {
    shiftType match {
      case 0 =>
        shift_type = shiftType
        shift_amount = immediate5
      case 1 =>
      case 2 =>
        shift_type = shiftType
        if (immediate5 == 0) shift_amount = 32 else shift_amount = immediate5
      case 3 =>
        if (immediate5 == 0) {
          shift_type = shiftType
          shift_amount = 1
        } else {
          shift_type = ROR
          shift_amount = immediate5
        }
      case _ =>
    }
  }

  // Leave shift ops for a bit
  def shift_carry(value : Int, shiftType : Int, amount: Int, carry_in : Int): Int = {
    if(amount == 0) {
      carry_out = carry_in
      return value
    } else {
      shiftType match {
        case 0 => 0
        case 1 => 0
        case 2 => 0
        case 3 => 0
        case 4 => 0
        case _ => 0
      }
    }
  }

  def isZeroBit(value : Int): Int = {
    if (value == 0) return 1 else 0
  }

  def conditionals(instruction: Int): Unit = {
    // retrieve conditional bits
    val cond = 0
    cond match {
      case 0 => return "eq"
      case 1 => return "ne"
      case 2 => return "cs"
      case 3 => return "cc"
      case 4 => return "mi"
      case 8 => return "hi"
      case 9 => return "ls"
      case 0xA => return "ge"
      case 0xB => return "lt"
      case 0xC => return "gt"
      case 0xD => return "le"
      case _ => return ""
    }
  }

}
