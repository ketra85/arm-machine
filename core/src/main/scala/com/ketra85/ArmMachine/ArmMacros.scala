package com.ketra85.ArmMachine
import scala.reflect.macros.blackbox
import scala.reflect.macros.blackbox.Context

// There is a massive need for re-organisation

class ArmMacros(em: Emulator) {

  def armUnknown(): Unit = macro armUnknownMacro

  def armUnknownMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         ${em.undefinedException()}
       """)
  }

  // Data processing ops except multiplication
  object ALU {
    var value: Int = 0
    var rd: Int = 0
    var carry_out: Boolean = false
    var inst: Int = 0

    def negative(i: Int): Int = i >> 31
    def positive(i: Int): Int = ~i >> 31


    def setConditionalLogical(res: Int): Unit = {
      em.N_FLAG = res < 0
      em.Z_FLAG = res == 0
      em.C_FLAG = carry_out
    }

    def setConditionalAdd(res: Int, lhs: Int, rhs: Int): Unit = {
      em.N_FLAG = res < 0
      em.Z_FLAG = res == 0
      em.V_FLAG = ((negative(lhs) & negative(rhs) & positive(res)) |
                  (positive(lhs) & positive(rhs) & negative(res))) == 1
      em.C_FLAG = ((negative(lhs) & negative(rhs)) |
                  (negative(lhs) & positive(res)) |
                  (negative(rhs) & positive(res))) == 1
    }

    def setConditionalSub(res: Int, lhs: Int, rhs: Int): Unit = {
      em.N_FLAG = res < 0
      em.Z_FLAG = res == 0
      em.V_FLAG = ((negative(lhs) & positive(rhs) & positive(res)) |
                  (positive(lhs) & negative(rhs) & negative(res))) == 1
      em.C_FLAG = ((negative(lhs) & positive(rhs)) |
                  (negative(lhs) & positive(res)) |
                  (positive(rhs) & positive(res))) == 1
    }

    def checkPc(setConditional: (Int, Int, Int) => Unit): Unit = {
      if(rd != 15) setConditional()
    }

    def checkPc(setConditional: Int => Unit): Unit = {
      if(rd != 15) setConditional()
    }

    def init(instruction: Int): Unit = {
      inst = instruction
      value = 0
      rd = (inst >> 12) & 0xf
      carry_out = em.cpsr.isSet(Flag.C)
    }

    def LSLImm(): Unit = {
      val shift = (inst >> 7) & 0x1f

      if (shift == 0) {
        value = em.registers(inst & 0x0f)
      } else {
        val v = em.registers(inst & 0x0f)
        carry_out = ((v >> (32 - shift)) & 1) == 1
        value = v << shift
      }
    }

    def LSLReg(): Unit = {
      val shift = em.registers((inst >> 8) & 15)
      var rm = em.registers(inst & 0x0f)

      if((inst & 0x0f) == 15) rm += 4

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
      val shift = (inst >> 7) & 0x1f

      if (shift == 0) {
        val v = em.registers(inst & 0x0f)
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = v >> shift
      } else {
        value = 0
        carry_out = (em.registers(inst & 0x0f) & 0x80000000) == 0x80000000
      }
    }

    def LSRReg(): Unit = {
      val shift = em.registers((inst >> 8) & 15)
      var rm = em.registers(inst & 0x0f)

      if((inst & 0x0f) == 15) rm += 4

      if(shift == 0) {
        if(shift == 32) {
          value = 0
          carry_out = (rm & 0x80000000) == 0x80000000
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
      val shift = (inst >> 7) & 0x1f

      if (shift == 0) {
        val v = em.registers(inst & 0x0f)
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = v >> shift
      } else {
        if((em.registers(inst & 0x0f) & 0x80000000) == 0x80000000) {
          value = 0xFFFFFFFF
          carry_out = true
        } else {
          value = 0
          carry_out = false
        }
      }
    }

    def ASRReg(): Unit = {
      val shift = em.registers((inst >> 8) & 15)
      var rm = em.registers(inst & 0x0f)

      if((inst & 0x0f) == 15) rm += 4

      if(shift < 32) {
        if(shift == 0) {
          val v = rm
          carry_out = ((v >> (shift - 1)) & 1) == 1
          value = v >> shift
        } else {
          value = rm
        }
      } else {
        if((em.registers(inst & 0x0f) & 0x80000000) == 0x80000000) {
          value = 0xFFFFFFFF
          carry_out = true
        } else {
          value = 0
          carry_out = false
        }
      }
    }

    def RORImm(): Unit = {
      val shift = (inst >> 7) & 0x1f
      val carry_in = if(em.C_FLAG) 1 else 0

      if (shift == 0) {
        val v = em.registers(inst & 0x0f)
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = (v << (32 - shift)) | (v >> shift)
      } else {
        val v = em.registers(inst & 0x0f)
        carry_out = ((v & 1)) == 1
        // assign these values?
        value = (v >> 1) | (carry_in << 31)
      }
    }

    def RORReg(): Unit = {
      val shift = em.registers((inst >> 8) & 15)
      var rm = em.registers(inst & 0x0f)

      if((inst & 0x0f) == 15) rm += 4

      if((shift & 0x1f) == 1) {
        val v = rm
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = (v << (32 -  shift)) | (v >> shift)
      } else {
        value = rm
        if(shift == 0) carry_out = (value & 0x80000000) == 0x80000000

      }
    }

    def valueImm(): Unit = {
      val shift = (inst & 0xf00) >> 7

      if (shift == 0) {
        val v = inst & 0xff
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = (v << (32 - shift)) | (v >> shift)
      } else {
        value = inst & 0xff
      }
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
    def orr(): Unit = macro orrMacro
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

    def tstMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val result = em.registers((inst >> 16) & 0x0f) & value
      c.Expr(
        q"""
          ${setConditionalLogical(result)}
        """)
    }

    def teqMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val result = em.registers((inst >> 16) & 0x0f) ^ value
      c.Expr(
        q"""
          ${setConditionalLogical(result)}
        """)
    }

    def cmpMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val rn = em.registers((inst >> 16) & 15)
      val imm = value
      val result = rn - imm
      c.Expr(
        q"""
           ${setConditionalSub(rn, imm, result)}
         """)
    }

    def cmnMacro(c: Context): c.Expr[Unit] = {
      import c.universe._

      val rn = em.registers((inst >> 16) & 15)
      val imm = value
      val result = rn + imm
      c.Expr(
        q"""
          ${setConditionalAdd(rn, imm, result)}
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

  def mrsCPSRMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if((${instruction.value} & 0x0fff0fff) == 0x014f0000) {
          ${em.updateCPSR()}
          ${em.registers((instruction.value >> 12) & 0x0f)} = ${em.registers(17)}
         } else {
          ${armUnknown()}
         }
       """)
  }

  def mrsSPSRMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if((${instruction.value} & 0x0fff0fff) == 0x010f0000) {
          ${em.updateCPSR()}
          ${em.registers((instruction.value >> 12) & 0x0f)} = ${em.registers(16)}
         } else {
          ${armUnknown()}
         }
       """)
  }

  def msrCPSRRegister(): Unit = macro msrCPSRRegisterMacro
  def msrSPSRRegister(): Unit = macro msrSPSRRegisterMacro

  def msrCPSRRegisterMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    var value: Int = 0
    var newValue: Int = 0

    c.Expr(
      q"""
         if((${instruction.value} & 0x0ff0fff0) == 0x0120f000) {
          ${em.updateCPSR()}
          $value = ${em.registers(instruction.value & 15)}
          $newValue = ${em.registers(16)}
          if(${em.currMode} != ${em.ProcessorMode.USR}) {
            if (${instruction.value} & 0x00010000) {
              $newValue = ($newValue & 0xffffff00) | ($value & 0x000000ff)
            }
            if (${instruction.value} & 0x00020000) {
              $newValue = ($newValue & 0xffff00ff) | ($value & 0x0000ff00)
            }
            if (${instruction.value} & 0x00040000) {
              $newValue = ($newValue & 0xff00ffff) | ($value & 0x00ff0000)
            }
          }
          if(${instruction.value} & 0x00080000) {
            $newValue = ($newValue & 0x00ffffff) | ($value & 0xff000000)
          }
          $newValue |= 0x10
          ${em.switchMode(em.ProcessorMode(newValue & 0x1f), save = false)}
          ${em.registers(16)} = newValue
          ${em.updateFlags()}
          if(!${em.armState}) {
            ${em.thumbPrefetch()}
            ${em.registers(15)} = ${em.nextPC} + 2
          }
         } else {
          ${armUnknown()}
         }
       """)
  }

  def msrSPSRRegisterMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    var value: Int = 0

    c.Expr(
      q"""
         if((${instruction.value} & 0x0ff0fff0) == 0x0160f000) {
          ${em.updateCPSR()}
          $value = ${em.registers(instruction.value & 15)}
          if((${em.currMode} != ${em.ProcessorMode.USR}) && (${em.currMode} != ${em.ProcessorMode.SYS})) {
            if (${instruction.value} & 0x00010000) {
              ${em.registers(17)} = (${em.registers(17)} & 0xffffff00) | ($value & 0x000000ff)
            }
            if (${instruction.value} & 0x00020000) {
              ${em.registers(17)} = (${em.registers(17)} & 0xffff00ff) | ($value & 0x0000ff00)
            }
            if (${instruction.value} & 0x00040000) {
              ${em.registers(17)} = (${em.registers(17)} & 0xff00ffff) | ($value & 0x00ff0000)
            }
            if (${instruction.value} & 0x00080000) {
              ${em.registers(17)} = (${em.registers(17)} & 0x00ffffff) | ($value & 0xff000000)
            }
          }
         } else {
          ${armUnknown()}
         }
       """)
  }


  def msrCPSRMultiple(): Unit = macro msrCPSRMultipleMacro
  def msrSPSRMultiple(): Unit = macro msrSPSRMultipleMacro


  // MSR CPSR_fields, #
  def msrCPSRMultipleMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    var value: Int = 0
    var v: Int = 0
    var newValue: Int = 0
    var shift: Int = 0

    c.Expr(
      q"""
         if((${instruction.value} & 0x0ff0f000) == 0x0320f000) {
          ${em.updateCPSR()}
          $value = ${instruction.value} & 0xff
          $shift = (${instruction.value} & 0xf00) >> 7
          if($shift == 0xf00) {
            $v = ${instruction.value} & 0xff
            $value = ($v << (32 - $shift)) | ($v >> $shift)
          }
          $newValue = ${em.registers(16)}
          if(${em.currMode} != ${em.ProcessorMode.USR}) {
            if(${instruction.value} & 0x00010000) {
              $newValue = ($newValue & 0xffffff00) | ($value & 0x000000ff)
            }
            if(${instruction.value} & 0x00020000) {
              $newValue = ($newValue & 0xffff00ff) | ($value & 0x0000ff00)
            }
            if(${instruction.value} & 0x00040000) {
              $newValue = ($newValue & 0xff00ffff) | ($value & 0x00ff0000)
            }
          }
          if(${instruction.value} & 0x00080000) {
            $newValue = ($newValue & 0x00ffffff) | ($value & 0xff000000)
          }
          $newValue |= 0x10
          ${em.switchMode(em.ProcessorMode(newValue & 0x1f), save = false)}
          ${em.registers(16)} = $newValue
          ${em.updateFlags()}
          if(!${em.armState}) {
            ${em.thumbPrefetch()}
            ${em.registers(15)} = ${em.nextPC} + 2
          }
        } else {
          ${armUnknown()}
        }
       """)
  }

  // MSR SPSR_fields, #
  def msrSPSRMultipleMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    var value: Int = 0
    var v: Int = 0
    var shift: Int = 0

    c.Expr(
      q"""
        if((${instruction.value} & 0x0ff0f000) == 0x0360f000) {
          if((${em.currMode} != ${em.ProcessorMode.USR}) && (${em.currMode} != ${em.ProcessorMode.SYS})) {
            $value = ${instruction.value} & 0xff
            $shift = (${instruction.value} & 0xf00) >> 7
            if($shift == 0xf00) {
              $v = ${instruction.value} & 0xff
              $value = ($v << (32 - $shift)) | ($v >> $shift)
            }
            if(${instruction.value} & 0x00010000) {
              ${em.registers(17)} = (${em.registers(17)} & 0xffffff00) | ($value & 0x000000ff)
            }
            if(${instruction.value} & 0x00020000) {
              ${em.registers(17)} = (${em.registers(17)} & 0xffff00ff) | ($value & 0x0000ff00)
            }
            if(${instruction.value} & 0x00040000) {
              ${em.registers(17)} = (${em.registers(17)} & 0xff00ffff) | ($value & 0x00ff0000)
            }
            if(${instruction.value} & 0x00080000) {
              ${em.registers(17)} = (${em.registers(17)} & 0x00ffffff) | ($value & 0xff000000)
            }
          }
        } else {
          ${armUnknown()}
        }
       """)
  }

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

  // STM and LDM
  def blockDataTransfer(instruction: Int): Unit = macro blockDataTransferMacro

  def blockDataTransferMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val rn: Int = (instruction.value >> 16) & 15
    val P: Boolean = ((instruction.value >> 24) & 1) == 1
    val U: Boolean = ((instruction.value >> 23) & 1) == 1
    val S: Boolean = ((instruction.value >> 22) & 1) == 1
    val W: Boolean = ((instruction.value >> 21) & 1) == 1
    val list: Int = (instruction.value & 0xffff)

    var regs: Byte = 0

    c.Expr(
      q"""

       """)
  }

  // Load and Store ops
  def loadStore(instruction: Int): Unit = macro loadStoreMacro

  def loadStoreMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val I: Boolean = ((instruction.value >> 25) & 1) == 1
    val P: Boolean = ((instruction.value >> 24) & 1) == 1
    val U: Boolean = ((instruction.value >> 23) & 1) == 1
    val B: Boolean = ((instruction.value >> 22) & 1) == 1
    val W: Boolean = ((instruction.value >> 21) & 1) == 1
    val L: Boolean = ((instruction.value >> 20) & 1) == 1

    var base: Int = (instruction.value >> 16) & 15
    val dest: Int = (instruction.value >> 12) & 15
    var offset: Int = 0
    var address: Int = base

    c.Expr(
      q"""
        if($P) {
          $address += (if($U) 1 else -1) * $offset
          if($B) {
            if($L) {
              $dest = ${em.memory.read8(address)}
            } else {
              ${em.memory.write8(address, (dest & 0xff).toByte)}
            }
          } else {
            if($L) {
              ${em.memory.read32(address)}
            } else {
              ${em.memory.write32(address, dest)}
            }
          }
          if($W && $base != 15) $base = $address
        } else {
          if($W) $dest = ${em.registers(dest)}
          if($B) {
            if($L) {
              $dest = ${em.memory.read8(address)}
            } else {
              ${em.memory.write8(address, (dest & 0xff).toByte)}
            }
          } else {
            if($L) {
              ${em.memory.read32(address)}
            } else {
              ${em.memory.write32(address, dest)}
            }
            if($W != 15) $base = $address + ((if($U) 1 else -1) * $offset)
          }
        }
       """)
  }

  object loadStore {
    var instruction: Int = 0
    var dest: Int = 0
    var base: Int = 0
    var address: Int = 0
    var offset: Int = 0

    def loadStoreInit(calculateOffset: () => Unit, calculateAddress: () => Int): Unit = {
      dest = (instruction >> 12) & 15
      base = (instruction >> 16) & 15
      // calc offset and address
      calculateOffset()
      address = calculateAddress()
    }

    def STRInit(calculateOffset: () => Unit,
                calculateAddress: () => Int,
                storeData: () => Unit,
                writeBack1: () => Unit,
                writeBack2: () => Unit
                ): Unit = {
      loadStoreInit(calculateOffset, calculateAddress)
      writeBack1()
      storeData()
      writeBack2()
    }

    def LDRInit(calculateOffset: () => Unit,
                calculateAddress: () => Int,
                loadData: () => Unit,
                writeBack: () => Unit
               ): Unit = {
      loadStoreInit(calculateOffset, calculateAddress)
      loadData()

      if(dest != base) {
        writeBack()
      }

      if(dest == 15) {
        em.registers(15) = 0xFFFFFFFC
        em.nextPC = em.registers(15)
        em.registers(15) += 4
        em.armPrefetch()
      }
    }

    def offsetImm(): Unit = offset = instruction & 0xfff
    def offsetImm8(): Unit = offset = (instruction & 0x0f) | ((instruction >> 4) & 0xf0)
    def offsetReg(): Unit = offset = em.registers(instruction & 0xf)
    def offsetLSL(): Unit = offset = em.registers(instruction & 0xf) << em.registers((instruction>>7) & 31)
    def offsetLSR(): Unit = {
      val shift: Int = (instruction & 7) & 31
      if(shift == 31) {
        offset = em.registers(instruction & 0xf) >> shift
      } else if((em.registers(instruction & 0xf) & 0x80000000) == 0x80000000) {
        offset = 0xffffffff
      } else {
        offset = 0
      }
    }
    def offsetASR(): Unit = {
      val shift: Int = (instruction >> 7) & 31

      if(shift == 31) {
        offset = em.registers(15) >> shift
      } else if((em.registers(instruction & 15) & 0x8000000) == 0x8000000) {
        offset = 0xffffffff
      } else {
        offset = 0
      }
    }
    def offsetROR(): Unit = {
      val shift: Int = (instruction & 7) & 31
      offset = em.registers(instruction & 15)
      if(shift == 31) {
        offset = (offset << (32) - shift) | (offset >> shift)
      } else {
        offset = (offset >> 1) | (if(em.C_FLAG) 1 << 31 else 0 << 31)
      }
    }

    // Addressing
    def postAddress(): Int = em.registers(base)
    def preDecAddress(): Int = em.registers(base) - offset
    def preIncAddress(): Int = em.registers(base) + offset

    def execSTR(): Unit = em.memory.write32(address, em.registers(dest))
    def execSTRH(): Unit = em.memory.write16(address, em.registers(dest).toShort)
    def execSTRB(): Unit = em.memory.write8(address, em.registers(dest).toByte)
    def execLDR(): Unit = em.registers(dest) = em.memory.read32(address)
    def execLDRH(): Unit = em.registers(dest) = em.memory.read16(address)
    def execLDRB(): Unit = em.registers(dest) = em.memory.read8(address)
    def execLDRSH(): Unit = em.registers(dest) = em.memory.read16(address) // signed
    def execLDRSB(): Unit = em.registers(dest) = em.memory.read8(address) // signed


    // Writeback
    def writeBackNone(): Unit = {}
    def writeBackPre(): Unit = em.registers(base) = address
    def writeBackPostDec(): Unit = em.registers(base) = address - offset
    def writeBackPostInc(): Unit = em.registers(base) = address + offset

    // STR[T] Rd, [Rn], -#
    def strPostDec(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, postAddress, storeData, writeBackNone, writeBackPostDec)
    }

    // STR[T] Rd, [Rn], #
    def strPostInc(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preDecAddress, storeData, writeBackNone, writeBackPostInc)
    }

    // STR Rd, [Rn, -#]
    def strPreDec(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preDecAddress, storeData, writeBackNone, writeBackNone)
    }

    // STR Rd, [Rn, -#]!
    def strPreDecWB(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preDecAddress, storeData, writeBackPre, writeBackNone)

    }

    // STR Rd, [Rn, #]
    def strPreInc(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preIncAddress, storeData, writeBackNone, writeBackNone)
    }

    // STR Rd, [Rn, #]!
    def strPreIncWB(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preIncAddress, storeData, writeBackPre, writeBackNone)
    }

    // LDR[T] Rd, [Rn], -#
    def ldrPostDec(calculateOffset: () => Unit,
                   loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, postAddress, loadData, writeBackPostDec)
    }

    // LDR Rd, [Rn], #
    def ldrPostInc(calculateOffset: () => Unit,
                   loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, postAddress, loadData, writeBackPostInc)
    }

    // LDR Rd, [Rn, -#]
    def ldrPreDec(calculateOffset: () => Unit,
                  loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, preDecAddress, loadData, writeBackNone)
    }

    // LDR Rd, [Rn, -#]!
    def ldrPreDecWB(calculateOffset: () => Unit,
                    loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, preDecAddress, loadData, writeBackPre)
    }

    // LDR Rd, [Rn, #]
    def ldrPreInc(calculateOffset: () => Unit,
                  loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, preIncAddress, loadData, writeBackNone)
    }

    // LDR Rd, [Rn, #]!
    def ldrPreIncWB(calculateOffset: () => Unit,
                    loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, preIncAddress, loadData, writeBackPre)
    }

    // STR Rd, [Rn, -#]
    def STROffsetImmPreDec(opcode: Int): Unit = {
      instruction = opcode
      strPreDec(offsetImm, execSTR)
    }

    // LDR Rd, [Rn, -#]
    def LDROffsetImmPreDec(opcode: Int): Unit = {
      instruction = opcode
      strPreDec(offsetImm, execLDR)
    }

    // STR Rd, [Rn, -#]!
    def STROffsetImmPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      strPreDecWB(offsetImm, execSTR)
    }

    // LDR Rd, [Rn, -#]!
    def LDROffsetImmPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      strPreDecWB(offsetImm, execLDR)
    }

    // STR Rd, [Rn, #]
    def STROffsetImmPreInc(opcode: Int): Unit = {
      instruction = opcode
      strPreInc(offsetImm, execSTR)
    }

    // LDR Rd, [Rn, #]
    def LDROffsetImmPreInc(opcode: Int): Unit = {
      instruction = opcode
      ldrPreInc(offsetImm, execLDR)
    }

    // STR Rd, [Rn, #]!
    def STROffsetImmPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      strPreIncWB(offsetImm, execSTR)
    }

    // LDR Rd, [Rn, #]!
    def LDROffsetImmPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      ldrPreIncWB(offsetImm, execLDR)
    }

    // STR[T] Rd, [Rn], -Rm, LSL #
    def STROffsetLSLPostDec(opcode: Int): Unit = {
      instruction = opcode
      strPostDec(offsetLSL, execSTR)
    }

    // STR[T] Rd, [Rn], -Rm, LSR #
    def STROffsetLSRPostDec(opcode: Int): Unit = {
      instruction = opcode
      strPostDec(offsetLSR, execSTR)
    }

    // STR[T] Rd, [Rn], -Rm, ASR #
    def STROffsetASRPostDec(opcode: Int): Unit = {
      instruction = opcode
      strPostDec(offsetASR, execSTR)
    }
    // STR[T] Rd, [Rn], -Rm, ROR #
    def STROffsetRORPostDec(opcode: Int): Unit = {
      instruction = opcode
      strPostDec(offsetROR, execSTR)
    }

    // LDR[T] Rd, [Rn], -Rm, LSL #
    def LDROffsetLSLPostDec(opcode: Int): Unit = {
      instruction = opcode
      ldrPostDec(offsetLSL, execLDR)
    }

    // LDR[T] Rd, [Rn], -Rm, LSR #
    def LDROffsetLSRPostDec(opcode: Int): Unit = {
      instruction = opcode
      ldrPostDec(offsetLSR, execLDR)
    }

    // LDR[T] Rd, [Rn], -Rm, ASR #
    def LDROffsetASRPostDec(opcode: Int): Unit = {
      instruction = opcode
      ldrPostDec(offsetASR, execLDR)
    }

    // LDR[T] Rd, [Rn], -Rm, ROR #
    def LDROffsetRORPostDec(opcode: Int): Unit = {
      instruction = opcode
      ldrPostDec(offsetROR, execLDR)
    }

    // STR[T] Rd, [Rn], Rm, LSL #
    def STROffsetLSLPostInc(opcode: Int): Unit = {
      instruction = opcode
      strPostInc(offsetLSL, execSTR)
    }

    // STR[T] Rd, [Rn], Rm, LSR #
    def STROffsetLSRPostInc(opcode: Int): Unit = {
      instruction = opcode
      strPostInc(offsetLSR, execSTR)
    }

    // STR[T] Rd, [Rn], Rm, ASR #
    def STROffsetASRPostInc(opcode: Int): Unit = {
      instruction = opcode
      strPostInc(offsetASR, execSTR)
    }
    // STR[T] Rd, [Rn], Rm, ROR #
    def STROffsetRORPostInc(opcode: Int): Unit = {
      instruction = opcode
      strPostInc(offsetROR, execSTR)
    }

    // LDR[T] Rd, [Rn], Rm, LSL #
    def LDROffsetLSLPostInc(opcode: Int): Unit = {
      instruction = opcode
      ldrPostInc(offsetLSL, execLDR)
    }

    // LDR[T] Rd, [Rn], Rm, LSR #
    def LDROffsetLSRPostInc(opcode: Int): Unit = {
      instruction = opcode
      ldrPostInc(offsetLSR, execLDR)
    }

    // LDR[T] Rd, [Rn], Rm, ASR #
    def LDROffsetASRPostInc(opcode: Int): Unit = {
      instruction = opcode
      ldrPostInc(offsetASR, execLDR)
    }

    // LDR[T] Rd, [Rn], Rm, ROR #
    def LDROffsetRORPostInc(opcode: Int): Unit = {
      instruction = opcode
      ldrPostInc(offsetROR, execLDR)
    }

    // STR Rd, [Rn, -Rm, LSL #]
    def STROffsetLSLPreDec(opcode: Int): Unit = {
      instruction = opcode
      strPreDec(offsetLSL, execSTR)
    }

    // STR Rd, [Rn, -Rm, LSR #]
    def STROffsetLSRPreDec(opcode: Int): Unit = {
      instruction = opcode
      strPreDec(offsetLSR, execSTR)
    }

    // STR Rd, [Rn, -Rm, ASR #]
    def STROffsetASRPreDec(opcode: Int): Unit = {
      instruction = opcode
      strPreDec(offsetASR, execSTR)
    }

    // STR Rd, [Rn, -Rm, ROR #]
    def STROffsetRORPreDec(opcode: Int): Unit = {
      instruction = opcode
      strPreDec(offsetROR, execSTR)
    }

    // LDR Rd, [Rn, -Rm, LSL #]
    def LDROffsetLSLPreDec(opcode: Int): Unit = {
      instruction = opcode
      ldrPreDec(offsetLSL, execLDR)
    }
    // LDR Rd, [Rn, -Rm, LSR #]
    def LDROffsetLSRPreDec(opcode: Int): Unit = {
      instruction = opcode
      ldrPreDec(offsetLSR, execLDR)
    }
    // LDR Rd, [Rn, -Rm, ASR #]
    def LDROffsetASRPreDec(opcode: Int): Unit = {
      instruction = opcode
      ldrPreDec(offsetASR, execLDR)
    }
    // LDR Rd, [Rn, -Rm, ROR #]
    def LDROffsetRORPreDec(opcode: Int): Unit = {
      instruction = opcode
      ldrPreDec(offsetROR, execLDR)
    }

    // STR Rd, [Rn, -Rm, LSL #]!
    def STROffsetLSLPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      strPreDecWB(offsetLSL, execSTR)
    }

    // STR Rd, [Rn, -Rm, LSR #]!
    def STROffsetLSRPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      strPreDecWB(offsetLSR, execSTR)
    }

    // STR Rd, [Rn, -Rm, ASR #]!
    def STROffsetASRPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      strPreDecWB(offsetASR, execSTR)
    }

    // STR Rd, [Rn, -Rm, ROR #]!
    def STROffsetRORPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      strPreDecWB(offsetROR, execSTR)
    }

    // LDR Rd, [Rn, -Rm, LSL #]!
    def LDROffsetLSLPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      ldrPreDecWB(offsetLSL, execLDR)
    }

    // LDR Rd, [Rn, -Rm, LSR #]!
    def LDROffsetLSRPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      ldrPreDecWB(offsetLSR, execLDR)
    }

    // LDR Rd, [Rn, -Rm, ASR #]!
    def LDROffsetASRPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      ldrPreDecWB(offsetASR, execLDR)
    }

    // LDR Rd, [Rn, -Rm, ROR #]!
    def LDROffsetRORPreDecWB(opcode: Int): Unit = {
      instruction = opcode
      ldrPreDecWB(offsetROR, execLDR)
    }

    // STR Rd, [Rn, Rm, LSL #]
    def STROffsetLSLPreInc(opcode: Int): Unit = {
      instruction = opcode
      strPreInc(offsetLSL, execSTR)
    }

    // STR Rd, [Rn, Rm, LSR #]
    def STROffsetLSRPreInc(opcode: Int): Unit = {
      instruction = opcode
      strPreInc(offsetLSR, execSTR)
    }

    // STR Rd, [Rn, Rm, ASR #]
    def STROffsetASRPreInc(opcode: Int): Unit = {
      instruction = opcode
      strPreInc(offsetASR, execSTR)
    }

    // STR Rd, [Rn, Rm, ROR #]
    def STROffsetRORPreInc(opcode: Int): Unit = {
      instruction = opcode
      strPreInc(offsetROR, execSTR)
    }

    // LDR Rd, [Rn, Rm, LSL #]
    def LDROffsetLSLPreInc(opcode: Int): Unit = {
      instruction = opcode
      ldrPreInc(offsetLSL, execLDR)
    }

    // LDR Rd, [Rn, Rm, LSR #]
    def LDROffsetLSRPreInc(opcode: Int): Unit = {
      instruction = opcode
      ldrPreInc(offsetLSR, execLDR)
    }

    // LDR Rd, [Rn, Rm, ASR #]
    def LDROffsetASRPreInc(opcode: Int): Unit = {
      instruction = opcode
      ldrPreInc(offsetASR, execLDR)
    }

    // LDR Rd, [Rn, Rm, ROR #]
    def LDROffsetRORPreInc(opcode: Int): Unit = {
      instruction = opcode
      ldrPreInc(offsetROR, execLDR)
    }

    // STR Rd, [Rn, Rm, LSL #]!
    def STROffsetLSLPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      strPreIncWB(offsetLSL, execSTR)
    }

    // STR Rd, [Rn, Rm, LSR #]!
    def STROffsetLSRPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      strPreIncWB(offsetLSR, execSTR)
    }

    // STR Rd, [Rn, Rm, ASR #]!
    def STROffsetASRPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      strPreIncWB(offsetASR, execSTR)
    }

    // STR Rd, [Rn, Rm, ROR #]!
    def STROffsetRORPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      strPreIncWB(offsetROR, execSTR)
    }

    // LDR Rd, [Rn, Rm, LSL #]!
    def LDROffsetLSLPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      ldrPreIncWB(offsetLSL, execLDR)
    }

    // LDR Rd, [Rn, Rm, LSR #]!
    def LDROffsetLSRPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      ldrPreIncWB(offsetLSR, execLDR)
    }

    // LDR Rd, [Rn, Rm, ASR #]!
    def LDROffsetASRPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      ldrPreIncWB(offsetASR, execLDR)
    }

    // LDR Rd, [Rn, Rm, ROR #]!
    def LDROffsetRORPreIncWB(opcode: Int): Unit = {
      instruction = opcode
      ldrPreIncWB(offsetROR, execLDR)
    }

    // STRH Rd, [Rn], -Rm
    def STRHOffsetRegPostDec(opcode: Int) {
      instruction = opcode
      strPostDec(offsetReg, execSTRH)
    }

    // STRH Rd, [Rn], #-offset
    def STRHOffsetImm8PostDec(opcode: Int) {
      instruction = opcode
      strPostDec(offsetImm8, execSTRH)
    }

    // STRH Rd, [Rn], Rm
    def STRHOffsetRegPostInc(opcode: Int) {
      instruction = opcode
      strPostInc(offsetReg, execSTRH)
    }

    // STRH Rd, [Rn], #offset
    def STRHOffsetImm8PostInc(opcode: Int) {
      instruction = opcode
      strPostInc(offsetImm8, execSTRH)
    }

    // STRH Rd, [Rn, -Rm]
    def STRHOffsetRegPreDec(opcode: Int) {
      instruction = opcode
      strPreDec(offsetReg, execSTRH)
    }

    // STRH Rd, [Rn, -Rm]!
    def STRHOffsetRegPreDecWB(opcode: Int) {
      instruction = opcode
      strPreDecWB(offsetReg, execSTRH)
    }

    // STRH Rd, [Rn, -#offset]
    def STRHOffsetImm8PreDec(opcode: Int) {
      instruction = opcode
      strPreDec(offsetImm8, execSTRH)
    }

    // STRH Rd, [Rn, -#offset]!
    def STRHOffsetImm8PreDecWB(opcode: Int) {
      instruction = opcode
      strPreDecWB(offsetImm8, execSTRH)
    }

    // STRH Rd, [Rn, Rm]
    def STRHOffsetRegPreInc(opcode: Int) {
      instruction = opcode
      strPreInc(offsetReg, execSTRH)
    }

    // STRH Rd, [Rn, Rm]!
    def STRHOffsetRegPreIncWB(opcode: Int) {
      instruction = opcode
      strPreIncWB(offsetReg, execSTRH)
    }

    // STRH Rd, [Rn, #offset]
    def STRHOffsetImm8PreInc(opcode: Int) {
      instruction = opcode
      strPreInc(offsetImm8, execSTRH)
    }

    // STRH Rd, [Rn, #offset]!
    def STRHOffsetImm8PreIncWB(opcode: Int) {
      instruction = opcode
      strPreIncWB(offsetImm8, execSTRH)
    }

    // LDRH Rd, [Rn], -Rm
    def LDRHOffsetRegPostDec(opcode: Int) {
      instruction = opcode
      ldrPostDec(offsetReg, execLDRH)
    }

    // LDRH Rd, [Rn], #-offset
    def LDRHOffsetImm8PostDec(opcode: Int) {
      instruction = opcode
      ldrPostDec(offsetImm8, execLDRH)
    }

    // lDRH Rd, [Rn], Rm
    def LDRHOffsetRegPostInc(opcode: Int) {
      instruction = opcode
      ldrPostInc(offsetReg, execLDRH)
    }

    // LDRH Rd, [Rn], #offset
    def LDRHOffsetImm8PostInc(opcode: Int) {
      instruction = opcode
      ldrPostInc(offsetImm8, execLDRH)
    }

    // LDRH Rd, [Rn, -Rm]
    def LDRHOffsetRegPreDec(opcode: Int) {
      instruction = opcode
      ldrPreDec(offsetReg, execLDRH)
    }

    // LDRH Rd, [Rn, -Rm]!
    def LDRHOffsetRegPreDecWB(opcode: Int) {
      instruction = opcode
      ldrPreDecWB(offsetReg, execLDRH)
    }

    // LDRH Rd, [Rn, -#offset]
    def LDRHOffsetImm8PreDec(opcode: Int) {
      instruction = opcode
      ldrPreDec(offsetImm8, execLDRH)
    }

    // LDRH Rd, [Rn, -#offset]!
    def LDRHOffsetImm8PreDecWB(opcode: Int) {
      instruction = opcode
      ldrPreDecWB(offsetImm8, execLDRH)
    }

    // LDRH Rd, [Rn, Rm]
    def LDRHOffsetRegPreInc(opcode: Int) {
      instruction = opcode
      ldrPreInc(offsetReg, execLDRH)
    }

    // LDRH Rd, [Rn, Rm]!
    def LDRHOffsetRegPreIncWB(opcode: Int) {
      instruction = opcode
      ldrPreIncWB(offsetReg, execLDRH)
    }

    // LDRH Rd, [Rn, #offset]
    def LDRHOffsetImm8PreInc(opcode: Int) {
      instruction = opcode
      ldrPreInc(offsetImm8, execLDRH)
    }

    // LDRH Rd, [Rn, #offset]!
    def LDRHOffsetImm8PreIncWB(opcode: Int) {
      instruction = opcode
      ldrPreIncWB(offsetImm8, execLDRH)
    }
  }

  // Branching
  def b(instruction: Int): Unit = macro bMacro
  def bl(instruction: Int): Unit = macro blMacro

  def mrc(): Unit = macro mrcMacro
  def swi(): Unit = macro swiMacro

  // Branch
  def bMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    var offset = instruction.value & 0x00ffffff
    c.Expr(
      q"""
        if(($offset & 0x00800000) == 0x00800000) $offset |= 0xff000000

        ${em.registers(15)} = $offset << 2
        ${em.nextPC} = ${em.registers(15)}
        ${em.registers(15)} += 4
        ${em.armPrefetch()}
      """)
  }

  // Branch with link
  def blMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    var offset = instruction.value & 0x00ffffff
    c.Expr(
      q"""
        if(($offset & 0x00800000) == 0x00800000) $offset |= 0xff000000

        ${em.registers(14)} = ${em.registers(15)} - 4
        ${em.registers(15)} = $offset << 2
        ${em.nextPC} = ${em.registers(15)}
        ${em.registers(15)} += 4
        ${em.armPrefetch()}
      """)
  }

  //mrc
  // unsupported
  def mrcMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""

       """)
  }

  // SWI
  def swiMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
        ${em.softwareInterrupt()}
       """)
  }

  def armExecuteMacro(): Int = {
    // Only twelve bits needed to decode
    // bits[27:20] (inclusive)
//    val identifier = (instruction >> 20) & 0x0ff
    val instruction: Int = em.prefetch(0)
    em.prefetch(0) = em.prefetch(1)
    val identifier = (instruction >> 25) & 7
    val op = (instruction >> 4) & 1
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
      identifier >> 1 match {
        // Data Processing ops
        case 0 => decodeDataProcessing(instruction)
        // Load and Store ops
        case 1 => loadStore(instruction)
        //branch ops
        case 2 => decodeBranch(instruction)
        case 3 => decodeCoProcessing(instruction)
        case _ => armUnknown()
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
    val op = (instruction >> 21) & 15
    val stateChange: Boolean = (op & 1) == 1
    ALU.init(instruction)

    op match {
      case 0 => ALU.and()
      case 1 => ALU.eor()
      case 2 => ALU.sub()
      case 3 => ALU.rsb()
      case 4 => ALU.add()
      case 5 => ALU.adc()
      case 6 => ALU.sbc()
      case 7 => ALU.rsc()
      case 8 => ALU.tst()
      case 9 => ALU.teq()
      case 10 => ALU.cmp()
      case 11 => ALU.cmn()
      case 12 => ALU.orr()
      case 13 => ALU.mov()
      case 14 => ALU.bic()
      case 15 => ALU.mvn()
    }
  }

  //offset and addressing calls
  def decodeLoadStore(instruction: Int): Unit = {
    val op = (instruction >> 20) & 0x0ff
    val I: Boolean = (op & 32) == 32
    val P: Boolean = (op & 16) == 16 // P == 1 Pre, P == 0 Post
    val U: Boolean = (op & 8) == 8 // U == 1 inc, U == 0 dec
    val B: Boolean = (op & 4) == 4 // B == 1 , B == 0
    val W: Boolean = (op & 2) == 2 // if P (W == 0) => offset addressing (base address not updated) (W == 1) => pre index addressing, if(P == 0) (W == 0) => LDR LDRB STR STRB (W == 1) => LDRT LDRBT STRBT STRT
    val L: Boolean = (op & 1) == 1 // L == 1 Load, L == 0 Store
    if(I) {
      op & 0x1f match {
        case 0 =>
        case 1 =>
        case 2 =>
        case 3 =>
        case 4 =>
        case 5 =>
        case 6 =>
        case 7 =>
        case 8 =>
        case 9 =>
        case 10 =>
        case 11 =>
        case 12 =>
        case 13 =>
        case 14 =>
        case 15 =>
        case 16 =>
        case 17 =>
        case 18 =>
        case 19 =>
        case 20 =>
        case 21 =>
        case 22 =>
        case 23 =>
        case 24 =>
        case 25 =>
        case 26 =>
        case 27 =>
        case 28 =>
        case 29 =>
        case 30 =>
        case 31 =>
      }
    } else {
      op & 0x1f match {
        case 0 =>
        case 1 =>
        case 2 =>
        case 3 =>
        case 4 =>
        case 5 =>
        case 6 =>
        case 7 =>
        case 8 =>
        case 9 =>
        case 10 =>
        case 11 =>
        case 12 =>
        case 13 =>
        case 14 =>
        case 15 =>
        case 16 =>
        case 17 =>
        case 18 =>
        case 19 =>
        case 20 =>
        case 21 =>
        case 22 =>
        case 23 =>
        case 24 =>
        case 25 =>
        case 26 =>
        case 27 =>
        case 28 =>
        case 29 =>
        case 30 =>
        case 31 =>
      }
    }
  }

  def decodeMultiply(instruction: Int): Unit = {
    var op: Int = (instruction >> 20) & 0xf

    op >> 1 match {
      case 1 => Multiplication.mul()
      case 2 => Multiplication.mla()
      case 4 => Multiplication.umull()
      case 5 => Multiplication.umlal()
      case 6 => Multiplication.smull()
      case 7 => Multiplication.smlal()
      case _ => armUnknown()
    }
  }

  def decodeBranch(instruction: Int): Unit = {
    val op = (instruction >> 25) & 7
    op match {
      case 1 => //todo
      case 5 => if((instruction & 0x01000000) == 1) bl(instruction) else b(instruction)
    }
  }

  def decodeMedia(instruction: Int): Unit = {
    val op = (instruction >> 20) & 0x0ff
//    op match {
//      case 1 => b()
//      case 0 => bl()
//    }
  }

  def decodeMisc(instruction: Int): Unit = {

  }

  def decodeCoProcessing(instruction: Int): Unit = {

  }
}
