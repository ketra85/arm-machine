package com.ketra85.ArmMachine

import org.graalvm.compiler.asm.aarch64.AArch64Assembler.Instruction

import scala.reflect.macros.blackbox
import scala.reflect.macros.blackbox.Context

// There is a massive need for re-organisation

class ArmMacros(em: Emulator) {

  var offset: Int
  var base: Int

  // Data processing ops except multiplication
  object ALU {
    var value: Int = 0
    var rd: Int = 0
    var carry_out: Boolean = false
    var inst: Int = 0

    def init(instruction: Int): Unit = {
      inst = instruction
      value = 0
      rd = (inst >> 12) & 0xf
      carry_out = em.cpsr.isSet(Flag.C)
    }

    def and(): Unit = macro andMacro
    def eor(): Unit = macro eorMacro
    def sub(): Unit = macro subMacro
    def rsb(): Unit = macro rsbMacro
    def add(): Unit = macro addMacro
    def adc(): Unit = macro adcMacro
    def rsc(): Unit = macro rscMacro
    def sbc(): Unit = macro sbcMacro
    def tst(): Unit = macro tstMacro
    def teq(): Unit = macro teqMacro
    def cmp(): Unit = macro cmpMacro
    def cmn(): Unit = macro cmnMacro
    def mov(): Unit = macro movMacro
    def bic(): Unit = macro bicMacro
    def mvn(): Unit = macro mvnMacro

    def andMacro(c: Context): c.Expr[Unit] = {
      import c.universe._
      val result = em.registers((inst >> 16) & 0xf) & value

      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

    def eorMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val result = em.registers((inst >> 16) & 0xf) ^ value
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)

    }

    def subMacro(c: Context): c.Expr[Unit] = {
      import c.universe._
      val rn = em.registers((inst >> 16) & 0xf)
      val imm = value
      val result = rn - imm
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

    def rsbMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val rn = em.registers((inst >> 16) & 0xf)
      val imm = value
      val result = imm - rn
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

    def addMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val rn = em.registers((inst >> 16) & 0xf)
      val imm = value
      val result = rn + imm
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

    def adcMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val rn = em.registers((inst >> 16) & 0xf)
      val imm = value
      val carry_in = if(em.C_FLAG) 0 else 1
      val result = rn + imm + carry_in
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

    def sbcMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val rn = em.registers((inst >> 16) & 0xf)
      val imm = value
      val carry_in = if(em.C_FLAG) 0 else 1
      val result = rn - imm - carry_in
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

    def rscMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val rn = em.registers((inst >> 16) & 0xf)
      val imm = value
      val carry_in = if(em.C_FLAG) 0 else 1
      val result = imm - rn - carry_in
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)

    }

    // incomplete need function to set flags
    def tstMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val result = em.registers((inst >> 16) & 0x0f) & value
      c.Expr(
        q"""

        """)
    }

    // incomplete
    def teqMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val result = em.registers((inst >> 16) & 0x0f) ^ value
      c.Expr(
        q"""

        """)
    }

    // incomplete
    def cmpMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val rn = em.registers((inst >> 16) & 0xf)
      val imm = value
      val result = rn - imm
      c.Expr(
        q"""

       """)

    }

    // incomplete
    def cmnMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val rn = em.registers((inst >> 16) & 0xf)
      val imm = value
      val result = rn + imm
      c.Expr(
        q"""

        """)
    }

    def orrMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val result = em.registers((inst >> 16) & 0x0f) | value
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

    def movMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val result = value
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

    def bicMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val result = em.registers((inst >> 16) & 0x0f) & (~value)
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

    def mvnMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val result = ~value
      c.Expr(
        q"""
          ${em.registers(rd)} = $result
        """)
    }

  }

  // Multiplication ops
  object Multiplication {
    var mult: Int = 0
    var rs: Int = 0
    var acc: Int = 0
    var dest: Int = 0
    var inst: Int = 0

    def init(instruction: Int): Unit = {
      inst = instruction
      mult = inst & 0x0f
      rs = em.registers((inst >> 8) & 0x0f)
      acc = (inst >> 12) & 0x0f
      dest = (inst >> 16) & 0x0f
      if(rs < 0) rs = ~rs
    }

    def mul(): Unit = macro mulMacro
    def mla(): Unit = macro mlaMacro
    def mull(): Unit = macro mullMacro
    def mlal(): Unit = macro mlalMacro
    def umull(): Unit = macro umullMacro
    def umlal(): Unit = macro umlalMacro
    def smull(): Unit = macro smullMacro
    def smlal(): Unit = macro smlalMacro

    def mulMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      c.Expr(
        q"""
           ${em.registers(dest)} = ${em.registers(mult)} * $rs
         """)
    }

    def mlaMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      c.Expr(
        q"""
           ${em.registers(dest)} = ${em.registers(mult)} * $rs + ${em.registers(acc)}
         """)
    }

    def mullMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val res = em.registers(mult) * rs
      c.Expr(
        q"""
          ${em.registers(acc)} = $res
          ${em.registers(dest)} = $res >> 32
         """)
    }

    def mlalMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val res = ((em.registers(dest) << 32) | em.registers(acc)) + em.registers(mult) * rs
      c.Expr(
        q"""
          ${em.registers(acc)} = $res
          ${em.registers(dest)} = $res >> 32
         """)
    }

    // incomplete
    def umullMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val res = ((em.registers(dest) << 32) | em.registers(acc)) + em.registers(mult) * rs
      c.Expr(
        q"""
          ${em.registers(acc)} = $res
          ${em.registers(dest)} = $res >> 32
         """)
    }

    // incomplete
    def umlalMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val res = ((em.registers(dest) << 32) | em.registers(acc)) + em.registers(mult) * rs
      c.Expr(
        q"""
          ${em.registers(acc)} = $res
          ${em.registers(dest)} = $res >> 32
         """)
    }

    // incomplete
    def smullMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val res = ((em.registers(dest) << 32) | em.registers(acc)) + em.registers(mult) * rs
      c.Expr(
        q"""
          ${em.registers(acc)} = $res
          ${em.registers(dest)} = $res >> 32
         """)
    }

    // incomplete
    def smlalMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val res = ((em.registers(dest) << 32) | em.registers(acc)) + em.registers(mult) * rs
      c.Expr(
        q"""
          ${em.registers(acc)} = $res
          ${em.registers(dest)} = $res >> 32
         """)
    }
  }


  // Data Processing shift operations
  object Shift {

    def LSLImm(): Unit = {
      val shift = (instruction >> 7) & 0x1f

      if (shift == 0) {
        value = em.registers(instruction & 0x0f)
      } else {
        val v = em.registers(instruction & 0x0f)
        carry_out = (((v >> (32 - shift)) & 1) == 1)
        value = v << shift
      }
    }

    def LSLReg(): Unit = {
      val shift = em.registers((instruction >> 8) & 15)
      var rm = em.registers(instruction & 0x0f)

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

    def LSRImm(): Unit = {
      val shift = (instruction >> 7) & 0x1f

      if (shift == 0) {
        val v = em.registers(instruction & 0x0f)
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = v >> shift
      } else {
        value = 0
        carry_out = (em.registers(instruction & 0x0f) & 0x80000000) == 1
      }
    }

    def LSRReg(): Unit = {
      val shift = em.registers((instruction >> 8) & 15)
      var rm = em.registers(instruction & 0x0f)

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

    def ASRImm(): Unit = {
      val shift = (instruction >> 7) & 0x1f

      if (shift == 0) {
        val v = em.registers(instruction & 0x0f)
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = v >> shift
      } else {
        if((em.registers(instruction & 0x0f) & 0x80000000) == 1) {
          value = 0xFFFFFFFF
          carry_out = true
        } else {
          value = 0
          carry_out = false
        }
      }
    }

    def ASRReg(): Unit = {
      val shift = em.registers((instruction >> 8) & 15)
      var rm = em.registers(instruction & 0x0f)

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
        if((em.registers(instruction & 0x0f) & 0x80000000) == 1) {
          value = 0xFFFFFFFF
          carry_out = true
        } else {
          value = 0
          carry_out = false
        }
      }
    }

    def RORImm(): Unit = {
      val shift = (instruction >> 7) & 0x1f
      val carry_in = if(em.C_FLAG) 1 else 0

      if (shift == 0) {
        val v = em.registers(instruction & 0x0f)
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = (v << (32 - shift)) | (v >> shift)
      } else {
        val v = em.registers(instruction & 0x0f)
        carry_out = ((v & 1)) == 1
        // assign these values?
        value = (v >> 1) | (carry_in << 31)
      }
    }

    def RORReg(): Unit = {
      val shift = em.registers((instruction >> 8) & 15)
      var rm = em.registers(instruction & 0x0f)

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

    def valueImm(): Unit = {
      val shift = (instruction & 0xf00) >> 7

      if (shift == 0) {
        val v = instruction & 0xff
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = (v << (32 - shift)) | (v >> shift)
      } else {
        value = instruction & 0xff
      }
    }

    //  def ROR_IMM_MSR(): Unit = {
    //    val v = instruction & 0xff
    //    value = ((v << (32 -shift)))
    //  }
  }


  object Offet {}

  // Misc ops
  def swp(instruction: Int): Unit = macro swpMacro
  def swpb(instruction: Int): Unit = macro swpbMacro

  def swpMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val address = em.registers((instruction.value >> 16) & 0xf)
    val temp = em.memory.read32(address)

    c.Expr(
      q"""
         ${em.memory.write32(address, em.registers(instruction.value & 0xf))}
         ${em.registers((instruction.value >> 12) & 0xf)} = $temp
       """)
  }

  def swpbMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val address = em.registers((instruction.value >> 16) & 0xf)
    val temp = em.memory.read8(address)

    c.Expr(
      q"""
        ${em.memory.write8(address, em.registers(instruction.value & 0xf).toByte)}
        ${em.registers((instruction.value >> 12) & 0xf)} = $temp
       """)
  }

  def mrsCPSR(): Unit = macro mrsCPSRMacro
  def mrsSPSR(): Unit = macro mrsSPSRMacro

  def msrCPSRRegister(): Unit = macro msrCPSRRegisterMacro
  def msrSPSRRegister(): Unit = macro msrSPSRRegisterMacro

  def msrCPSRMultiple(): Unit = macro msrMultipleMacro
  def msrSPSRMultiple(): Unit = macro msrMultipleMacro

  def bx(instruction: Int): Unit = macro bxMacro

  def bxMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val base = instruction.value & 0x0f
    c.Expr(
      q"""
        ${em.armState} = if((${em.registers(base)} & 1) == 1) false else true
        if(${em.armState}) {
          ${em.registers(15)} = ${em.registers(base)} & 0xFFFFFFFC
          ${em.armPrefetchNext()} = ${em.registers(15)}
          ${em.registers(15)} += 4
          ${em.armPrefetch()}
        } else {
          ${em.registers(15)} = ${em.registers(base)} & 0xFFFFFFFE
          ${em.armPrefetchNext()} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
        }
       """)
  }



  // Load and Store ops
  def ldr(): Unit = macro ldrMacro
  def ldrh(): Unit = macro ldrhMacro
  def ldrb(): Unit = macro ldrbMacro
  def ldrbt(): Unit = macro ldrbtMacro
  def ldrsh(): Unit = macro ldrshMacro
  def ldrsb(): Unit = macro ldrsbMacro
  def ldrt(): Unit = macro ldrtMacro
  def str(): Unit = macro strMacro
  def strh(): Unit = macro strhMacro
  def strb(): Unit = macro strbMacro
  def strbt(): Unit = macro strbtMacro
  def strt(): Unit = macro strtMacro

  // STM and LDM

  // Branching
  def b(): Unit = macro bMacro
  def bl(): Unit = macro blMacro

  def mrc(): Unit = macro mrcMacro
  def swi(): Unit = macro swiMacro





  def offsetImm(): Unit = offset = instruction & 0xfff
  def offsetImm8(): Unit = offset = (instruction & 0x0f) | ((instruction >> 4) & 0xf0)
  def offsetReg(): Unit = offset = em.registers(instruction & 0xf)
  def offsetLSL(): Unit = offset = em.registers(instruction & 0xf) << em.registers((instruction>>7) & 31)
  def offsetLSR(): Unit = {
    val shift: Int = (instruction & 7) & 31
    if(shift == 1) {
      offset = em.registers(instruction & 0xf) >> shift
    } else if((em.registers(instruction & 0xf) & 0x80000000) == 1) {
      offset = 0xffffffff
    } else {
      offset = 0
    }
  }
  def offsetASR(): Unit = {
    val shift: Int = (instruction & 7) & 31
    offset = em.registers(instruction & 0xf)

    // call ror offset methods
    if(shift == 1) 1 else 0
  }
  def offsetROR(): Unit = {
    val shift: Int = (instruction & 7) & 31
    offset = if(shift == 1) em.registers(instruction & 0xf) >> shift else 0
  }

  //
  def postAddress(): Int = em.registers(base)

  def preDecAddress(): Int = em.registers(base - offset)

  def preIncAddress(): Int = em.registers(base + offset)

  // Load and Store instructions
  def ldrMacro(instruction: Int): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
        ${em.registers(rd)} = ${em.memory.read32(0)}
       """)
  }


  def ldrbMacro(instruction: Int): Unit = {

  }

  def ldrbtMacro(instruction: Int): Unit = {

  }

  def ldrhMacro(instruction: Int): Unit = {

  }

  def ldrsbMacro(instruction: Int): Unit = {

  }

  def ldrshMacro(instruction: Int): Unit = {

  }

  def ldrtMacro(instruction: Int): Unit = {

  }

  def strMacro(instruction: Int): Unit = {

  }

  def strbMacro(instruction: Int): Unit = {

  }

  def strbtMacro(instruction: Int): Unit = {

  }

  def strhMacro(instruction: Int): Unit = {

  }

  def strtMacro(instruction: Int): Unit = {

  }


  // incomplete
  def bMacro(c: Context)(): Unit = {
    var offset = instruction & 0x00ffffff
    if((offset & 0x00800000) == 1) {
      offset |= 0xff000000
    } else {
      em.registers(15) = offset << 2

      em.registers(15) += 4

    }
  }

  // incomplete
  def blMacro(c: Context)(): Unit = {
  }

  // incomplete
  def swiMacro(c: Context)(): Unit = {

  }

  def armExecuteMacro(instruction: Int): Int = {
    val identifier = getMask(27, 25)
    val cond = instruction >> 28
    var cond_result = true
    if(cond != 0x0e) {
      cond match {
        case 0x00 => cond_result = em.Z_FLAG
        case 0x01 => cond_result = !em.Z_FLAG
        case 0x02 => cond_result = em.C_FLAG
        case 0x03 => cond_result = !em.C_FLAG
        case 0x04 => cond_result = em.N_FLAG
        case 0x05 => cond_result = !em.N_FLAG
        case 0x06 => cond_result = em.V_FLAG
        case 0x07 => cond_result = !em.V_FLAG
        case 0x08 => cond_result = em.C_FLAG && !em.Z_FLAG
        case 0x09 => cond_result = !em.C_FLAG || em.Z_FLAG
        case 0x0A => cond_result = em.N_FLAG == em.V_FLAG
        case 0x0B => cond_result = em.N_FLAG != !em.V_FLAG
        case 0x0C => cond_result = !em.Z_FLAG && (em.N_FLAG == em.V_FLAG)
        case 0x0D => cond_result = em.Z_FLAG || (em.N_FLAG != em.V_FLAG)
        case 0x0E => cond_result = true
        case 0x0F =>
        case _ => cond_result = false
      }
    }

    if(cond_result) {
      identifier match {
        // Data Processing ops
        case 0 || 1 => decodeDataProcessing(instruction)
        case 2 || 3 || 4 => decodeLoadStore(instruction)
        //branch case
        case 5 => decodeBranch(instruction)
        // Coprocesso
        case 6 || 7 => decodeCoProcessing(instruction)
      }
    }

    return 1
  }

  def getMask(a: Int, b: Int): Int = {
    var mask: Int = 0
    for(i: Int <- a; if i <= b) {
      mask |= i << 1
    }
    return mask
  }

  // call shift operands
  def decodeDataProcessing(instruction: Int): Unit = {
    val op = (instruction >> 20) & 0x1f
    ALU.init(instruction)

    (op >> 1) match {
      case 0 => ALU.and()
      case 2 => ALU.sub()
      case 4 => ALU.add()
    }
  }

  //offset and addressing calls
  def decodeLoadStore(instruction: Int): Unit = {

  }

  def decodeMultiply(instruction: Int): Unit = {

  }

  def decodeBranch(instruction: Int): Unit = {

  }

  def decodeMisc(instruction: Int): Unit = {

  }
}
