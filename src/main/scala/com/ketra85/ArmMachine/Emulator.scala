package com.ketra85.ArmMachine

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context


// Emulator class encapsulating emulator operations
// Memory, Registers, etc
class Emulator() {
//  private val DEFAULT_NUMBER_OF_GENERAL_REGISTERS = 15
//  private val DEFAULT_PC_REGISTERS = 1
//  private val DEFAULT_FLAG_REGISTERS = 2


  // reg 13 -> Stack Pointer
  // reg 14 -> Link Register
  // reg 15 -> Program Counter
  // BANKED AND UNBANKED REGS
  val USR_MODE = 0x10
  val FIQ_MODE = 0x11
  val IRQ_MODE = 0x12
  val SVC_MODE = 0x13
  val ABT_MODE = 0x17
  val UND_MODE = 0x1b
  val SYS_MODE = 0x1f

  var registers = Array.fill[Int](16)(0)
  // what do?
  var registersSYS = Array.fill[Int](16)(0)
  var registersSVC = Array.fill[Int](15)(0)
  var registersABT = Array.fill[Int](15)(0)
  var registersUND = Array.fill[Int](15)(0)
  var registersIRQ = Array.fill[Int](15)(0)
  var registersIRQ = Array.fill[Int](15)(0)

  var memory = Array.fill[Int](65536)(0)

  //remember multiple SPSR
  var cpsr = Map("n" -> 0, "z" -> 0, "c" -> 0, "v" -> 0, "q" -> 0,
    "e" -> 0, "a" -> 0, "i" -> 0, "f" -> 0, "t" -> 0, "m" -> 0)
  var spsrSVC = Map("n" -> 0, "z" -> 0, "c" -> 0, "v" -> 0, "q" -> 0,
    "e" -> 0, "a" -> 0, "i" -> 0, "f" -> 0, "t" -> 0, "m" -> 0)
  var spsrABT = Map("n" -> 0, "z" -> 0, "c" -> 0, "v" -> 0, "q" -> 0,
    "e" -> 0, "a" -> 0, "i" -> 0, "f" -> 0, "t" -> 0, "m" -> 0)
  var spsrUND = Map("n" -> 0, "z" -> 0, "c" -> 0, "v" -> 0, "q" -> 0,
    "e" -> 0, "a" -> 0, "i" -> 0, "f" -> 0, "t" -> 0, "m" -> 0)
  var spsrIRQ = Map("n" -> 0, "z" -> 0, "c" -> 0, "v" -> 0, "q" -> 0,
    "e" -> 0, "a" -> 0, "i" -> 0, "f" -> 0, "t" -> 0, "m" -> 0)
  var spsrFIQ = Map("n" -> 0, "z" -> 0, "c" -> 0, "v" -> 0, "q" -> 0,
    "e" -> 0, "a" -> 0, "i" -> 0, "f" -> 0, "t" -> 0, "m" -> 0)

  val ASR = 2
  val LSL = 0
  val LSR = 1
  val ROR = 4
  val RRX = 3

  var shift_type = 0
  var shift_amount = 0
  var carry_out : Boolean = false
  var overflow : Int = 0
  var branch = 0

  def getProgramCounter() : Int = {
    registers(15) + 8
  }

  def getRegister(index : Int) : Int = {
    if(index == 15) {
      getProgramCounter()
    } else {
      registers(index)
    }
  }

  def setRegister(index: Int, value: Int): Unit = {
    registers(index) = value
  }

  def getCPSR(): Map[String, Int] = {
    cpsr
  }

  def setCPSR(value : Int, set_overflow: Boolean): Unit = {
    cpsr("n") = value >>> 31
    cpsr("z") = if (value == 0) 1 else 0
    cpsr("c") = carry_out

    if(set_overflow) {
      cpsr("v") = overflow
    }
  }

  def conditionals(instruction: Int): String = {
    // retrieve conditional bits
    val cond = 0
    cond match {
      case 0 => "eq"
      case 1 => "ne"
      case 2 => "cs"
      case 3 => "cc"
      case 4 => "mi"
      case 8 => "hi"
      case 9 => "ls"
      case 0xA => "ge"
      case 0xB => "lt"
      case 0xC => "gt"
      case 0xD => "le"
      case _ => ""
    }
  }

  def LSLImm(instruction: Int): Unit = {
    val shift = (instruction >> 7) & 0x1f
    var value = 0

    if (shift == 0) {
      value = registers(instruction & 0x0f)
    } else {
      val v = registers(instruction & 0x0f)
      carry_out = (((v >> (32 - shift)) & 1) == 1)
      value = v << shift
    }
  }

  def LSLReg(instruction: Int): Unit = {
    val shift = registers((instruction >> 8) & 15)
    var rm = registers(instruction & 0x0f)
    var value = 0

    if((instruction & 0x0f) == 15) rm += 4

    if(shift == 0) {
      if(shift == 32) {
        value = 0
        carry_out = (rm & 1) == 1
      } else if(shift < 32) {
        val v = rm
        carry_out = ((v >> (32 - shift)) & 1) == 1
        value = v << shift
      } else {
        value = 0
        carry_out = false
      }
    } else {
      value = rm
    }
  }

  def LSRImm(instruction: Int): Unit = {
    val shift = (instruction >> 7) & 0x1f
    var value = 0

    if (shift == 0) {
      val v = registers(instruction & 0x0f)
      carry_out = ((v >> (shift - 1)) & 1) == 1
      value = v >> shift
    } else {
      value = 0
      carry_out = (registers(instruction & 0x0f) & 0x80000000) == 1
    }
  }

  def LSRReg(instruction: Int): Unit = {
    val shift = registers((instruction >> 8) & 15)
    var rm = registers(instruction & 0x0f)
    var value = 0

    if((instruction & 0x0f) == 15) rm += 4

    if(shift == 0) {
      if(shift == 32) {
        value = 0
        carry_out = (rm & 0x80000000) == 1
      } else if(shift < 32) {
        val v = rm
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = v >> shift
      } else {
        value = 0
        carry_out = false
      }
    } else {
      value = rm
    }
  }

  def ASRImm(instruction: Int): Unit = {
    val shift = (instruction >> 7) & 0x1f
    var value = 0

    if (shift == 0) {
      val v = registers(instruction & 0x0f)
      carry_out = ((v >> (shift - 1)) & 1) == 1
      value = v >> shift
    } else {
      if((registers(instruction & 0x0f) & 0x80000000) == 1) {
        value = 0xFFFFFFFF
        carry_out = true
      } else {
        value = 0
        carry_out = false
      }
    }
  }

  def ASRReg(instruction: Int): Unit = {
    val shift = registers((instruction >> 8) & 15)
    var rm = registers(instruction & 0x0f)
    var value = 0

    if((instruction & 0x0f) == 15) rm += 4

    if(shift < 32) {
      if(shift == 0) {
        val v = rm
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = v >> shift
      } else {
        value = rm
      }
    } else {
      if((registers(instruction & 0x0f) & 0x80000000) == 1) {
        value = 0xFFFFFFFF
        carry_out = true
      } else {
        value = 0
        carry_out = false
      }
    }
  }

  def RORImm(instruction: Int): Unit = {
    val shift = (instruction >> 7) & 0x1f
    var value = 0

    if (shift == 0) {
      val v = registers(instruction & 0x0f)
      carry_out = ((v >> (shift - 1)) & 1) == 1
      value = (v << (32 - shift)) | (v >> shift)
    } else {
      val v = registers(instruction & 0x0f)
      carry_out = ((v & 1)) == 1
      value = (v >> 1) | (cpsr("c") << 31)
    }
  }

  def RORReg(instruction: Int): Unit = {
    val shift = registers((instruction >> 8) & 15)
    var rm = registers(instruction & 0x0f)
    var value = 0

    if((instruction & 0x0f) == 15) rm += 4

    if((shift & 0x1f) == 1) {
      val v = rm
      carry_out = ((v >> (shift - 1)) & 1) == 1
      value = (v << (32 -  shift)) | (v >> shift)
    } else {
      value = rm
      if(shift == 0) carry_out = ((value & 0x80000000) == 1)

    }
  }

  def ValueImm(instruction: Int): Unit = {
    val shift = (instruction & 0xf00) >> 7
    var value = 0

    if (shift == 0) {
      val v = instruction & 0xff
      carry_out = ((v >> (shift - 1)) & 1) == 1
      value = (v << (32 - shift)) | (v >> shift)
    } else {
      value = instruction & 0xff
    }
  }

  object InstructionMacros {

    def adcImm(instruction: Int): Unit = macro adcImmMacro
    def adcReg(instruction: Int): Unit = macro adcRegMacro
    def addImm(instruction: Int): Unit = macro addImmMacro
    def addReg(instruction: Int): Unit = macro addRegMacro


    def adcImmMacro(c: Context)(instruction: Int): Unit = {
//      import c.universe._

      val rn = (instruction >>> 16) & 0xf
      val rd = (instruction >>> 12) & 0xf
      val sBit = instruction & 0x00100000
      val imm12 = instruction & 0xfff
      val result = registers(rn) + imm12 + cpsr("c")

      registers(rd) = result

    }

    def adcRegMacro(instruction: Int): Unit = {
      val sBit = instruction & 0x00100000
      val rn = (instruction >>> 16) & 0xf
      val rd = (instruction >>> 12) & 0xf
      val imm5 = (instruction >>> 7) & 0x1f
      val shiftType = (instruction >>> 5) & 3
      val rm = instruction & 0xf


    }
  }

}
