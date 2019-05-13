package com.ketra85.ArmMachine
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

    def LSLImm(): Unit = {
      val shift = (inst >> 7) & 0x1f

      if (shift == 0) {
        value = em.registers(inst & 0x0f)
      } else {
        val v = em.registers(inst & 0x0f)
        carry_out = (((v >> (32 - shift)) & 1) == 1)
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
        carry_out = (em.registers(inst & 0x0f) & 0x80000000) == 1
      }
    }

    def LSRReg(): Unit = {
      val shift = em.registers((inst >> 8) & 15)
      var rm = em.registers(inst & 0x0f)

      if((inst & 0x0f) == 15) rm += 4

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
      val shift = (inst >> 7) & 0x1f

      if (shift == 0) {
        val v = em.registers(inst & 0x0f)
        carry_out = ((v >> (shift - 1)) & 1) == 1
        value = v >> shift
      } else {
        if((em.registers(inst & 0x0f) & 0x80000000) == 1) {
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
        if((em.registers(inst & 0x0f) & 0x80000000) == 1) {
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
        if(shift == 0) carry_out = ((value & 0x80000000) == 1)

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

    //  def ROR_IMM_MSR(): Unit = {
    //    val v = instruction & 0xff
    //    value = ((v << (32 -shift)))
    //  }

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

  // STM and LDM

  // Branching
  def b(instruction: Int): Unit = macro bMacro
  def bl(instruction: Int): Unit = macro blMacro

  def mrc(): Unit = macro mrcMacro
  def swi(): Unit = macro swiMacro

  // Load and Store ops
  object loadStore {
    val instruction: Int = 0
    var dest: Int = 0
    var base: Int = 0
    var address: Int = 0
    var offset: Int = 0

    def loadStoreInit(calculateOffset: () => Unit, calculateAddress: () => Int): Unit = {
      dest = (instruction >> 12) & 15
      base = (instruction >> 15) & 15
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
      if(shift == 1) {
        offset = em.registers(instruction & 0xf) >> shift
      } else if((em.registers(instruction & 0xf) & 0x80000000) == 1) {
        offset = 0xffffffff
      } else {
        offset = 0
      }
    }
    def offsetASR(): Unit = {
      val shift: Int = (instruction >> 7) & 31
      offset = em.registers(instruction & 0xf)

      // call ror offset methods
      if(shift == 1) 1 else 0
    }
    def offsetROR(): Unit = {
      val shift: Int = (instruction & 7) & 31
      offset = if(shift == 1) em.registers(instruction & 0xf) >> shift else 0
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

    def strPostDec(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, postAddress, storeData, writeBackNone, writeBackPostDec)
    }
    def strPostInc(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preDecAddress, storeData, writeBackNone, writeBackPostInc)
    }
    def strPreDec(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preDecAddress, storeData, writeBackNone, writeBackNone)
    }
    def strPreDecWB(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preDecAddress, storeData, writeBackPre, writeBackNone)

    }
    def strPreInc(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preIncAddress, storeData, writeBackNone, writeBackNone)
    }
    def strPreIncWB(calculateOffset: () => Unit,
                   storeData: () => Unit
                  ): Unit = {
      STRInit(calculateOffset, preIncAddress, storeData, writeBackPre, writeBackNone)
    }
    def ldrPostDec(calculateOffset: () => Unit,
                   loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, postAddress, loadData, writeBackPostDec)
    }
    def ldrPostInc(calculateOffset: () => Unit,
                   loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, postAddress, loadData, writeBackPostInc)
    }
    def ldrPreDec(calculateOffset: () => Unit,
                  loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, preDecAddress, loadData, writeBackNone)
    }
    def ldrPreDecWB(calculateOffset: () => Unit,
                    loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, preDecAddress, loadData, writeBackPre)
    }
    def ldrPreInc(calculateOffset: () => Unit,
                  loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, preIncAddress, loadData, writeBackNone)
    }
    def ldrPreIncWB(calculateOffset: () => Unit,
                    loadData: () => Unit
                  ): Unit = {
      LDRInit(calculateOffset, preIncAddress, loadData, writeBackPre)
    }

    // STR Rd, [Rn, -#]
    def STROffsetImmPreDec(instruction: Int): Unit = {
      strPreDec(offsetImm, execSTR)
    }

    // LDR Rd, [Rn, -#]
    def LDROffsetImmPreDec(instruction: Int): Unit = {
      strPreDec(offsetImm, execLDR)
    }

    // STR Rd, [Rn, -#]!
    def STROffsetImmPreDecWB(instruction: Int): Unit = {
      strPreDecWB(offsetImm, execSTR)
    }

    // LDR Rd, [Rn, -#]!
    def LDROffsetImmPreDecWB(instruction: Int): Unit = {
      strPreDecWB(offsetImm, execLDR)
    }

    // STR Rd, [Rn, #]
    def STROffsetImmPreInc(instruction: Int): Unit = {
      strPreInc(offsetImm, execSTR)
    }

    // LDR Rd, [Rn, #]
    def LDROffsetImmPreInc(instruction: Int): Unit = {
      ldrPreInc(offsetImm, execLDR)
    }

    // STR Rd, [Rn, #]!
    def STROffsetImmPreIncWB(instruction: Int): Unit = {
      strPreIncWB(offsetImm, execSTR)
    }

    // LDR Rd, [Rn, #]!
    def LDROffsetImmPreIncWB(instruction: Int): Unit = {
      ldrPreIncWB(offsetImm, execLDR)
    }

    // STR[T] Rd, [Rn], -Rm, LSL #
    def STROffsetLSLPostDec(instruction: Int): Unit = {
      strPostDec(offsetLSL, execSTR)
    }

    // STR[T] Rd, [Rn], -Rm, LSR #
    def STROffsetLSRPostDec(instruction: Int): Unit = {
      strPostDec(offsetLSR, execSTR)
    }

    // STR[T] Rd, [Rn], -Rm, ASR #
    def STROffsetASRPostDec(instruction: Int): Unit = {
      strPostDec(offsetASR, execSTR)
    }
    // STR[T] Rd, [Rn], -Rm, ROR #
    def STROffsetRORPostDec(instruction: Int): Unit = {
      strPostDec(offsetROR, execSTR)
    }

    // LDR[T] Rd, [Rn], -Rm, LSL #
    def LDROffsetLSLPostDec(instruction: Int): Unit = {
      ldrPostDec(offsetLSL, execLDR)
    }

    // LDR[T] Rd, [Rn], -Rm, LSR #
    def LDROffsetLSRPostDec(instruction: Int): Unit = {
      ldrPostDec(offsetLSR, execLDR)
    }

    // LDR[T] Rd, [Rn], -Rm, ASR #
    def LDROffsetASRPostDec(instruction: Int): Unit = {
      ldrPostDec(offsetASR, execLDR)
    }

    // LDR[T] Rd, [Rn], -Rm, ROR #
    def LDROffsetRORPostDec(instruction: Int): Unit = {
      ldrPostDec(offsetROR, execLDR)
    }

    // STR[T] Rd, [Rn], Rm, LSL #
    def STROffsetLSLPostInc(instruction: Int): Unit = {
      strPostInc(offsetLSL, execSTR)
    }

    // STR[T] Rd, [Rn], Rm, LSR #
    def STROffsetLSRPostInc(instruction: Int): Unit = {
      strPostInc(offsetLSR, execSTR)
    }

    // STR[T] Rd, [Rn], Rm, ASR #
    def STROffsetASRPostInc(instruction: Int): Unit = {
      strPostInc(offsetASR, execSTR)
    }
    // STR[T] Rd, [Rn], Rm, ROR #
    def STROffsetRORPostInc(instruction: Int): Unit = {
      strPostInc(offsetROR, execSTR)
    }

    // LDR[T] Rd, [Rn], Rm, LSL #
    def LDROffsetLSLPostInc(instruction: Int): Unit = {
      ldrPostInc(offsetLSL, execLDR)
    }

    // LDR[T] Rd, [Rn], Rm, LSR #
    def LDROffsetLSRPostInc(instruction: Int): Unit = {
      ldrPostInc(offsetLSR, execLDR)
    }

    // LDR[T] Rd, [Rn], Rm, ASR #
    def LDROffsetASRPostInc(instruction: Int): Unit = {
      ldrPostInc(offsetASR, execLDR)
    }

    // LDR[T] Rd, [Rn], Rm, ROR #
    def LDROffsetRORPostInc(instruction: Int): Unit = {
      ldrPostInc(offsetROR, execLDR)
    }

    // STR Rd, [Rn, -Rm, LSL #]
    def STROffsetLSLPreDec(instruction: Int): Unit = {
      strPreDec(offsetLSL, execSTR)
    }

    // STR Rd, [Rn, -Rm, LSR #]
    def STROffsetLSRPreDec(instruction: Int): Unit = {
      strPreDec(offsetLSR, execSTR)
    }

    // STR Rd, [Rn, -Rm, ASR #]
    def STROffsetASRPreDec(instruction: Int): Unit = {
      strPreDec(offsetASR, execSTR)
    }

    // STR Rd, [Rn, -Rm, ROR #]
    def STROffsetRORPreDec(instruction: Int): Unit = {
      strPreDec(offsetROR, execSTR)
    }

    // LDR Rd, [Rn, -Rm, LSL #]
    def LDROffsetLSLPreDec(instruction: Int): Unit = {
      ldrPreDec(offsetLSL, execLDR)
    }
    // LDR Rd, [Rn, -Rm, LSR #]
    def LDROffsetLSRPreDec(instruction: Int): Unit = {
      ldrPreDec(offsetLSR, execLDR)
    }
    // LDR Rd, [Rn, -Rm, ASR #]
    def LDROffsetASRPreDec(instruction: Int): Unit = {
      ldrPreDec(offsetASR, execLDR)
    }
    // LDR Rd, [Rn, -Rm, ROR #]
    def LDROffsetRORPreDec(instruction: Int): Unit = {
      ldrPreDec(offsetROR, execLDR)
    }

    // STR Rd, [Rn, -Rm, LSL #]!
    def STROffsetLSLPreDecWB(instruction: Int): Unit = {
      strPreDecWB(offsetLSL, execSTR)
    }
    // STR Rd, [Rn, -Rm, LSR #]!
    def STROffsetLSRPreDecWB(instruction: Int): Unit = {
      strPreDecWB(offsetLSR, execSTR)
    }
    // STR Rd, [Rn, -Rm, ASR #]!
    def STROffsetASRPreDecWB(instruction: Int): Unit = {
      strPreDecWB(offsetASR, execSTR)
    }
    // STR Rd, [Rn, -Rm, ROR #]!
    def STROffsetRORPreDecWB(instruction: Int): Unit = {
      strPreDecWB(offsetROR, execSTR)
    }

    // LDR Rd, [Rn, -Rm, LSL #]!
    def LDROffsetLSLPreDecWB(instruction: Int): Unit = {
      ldrPreDecWB(offsetLSL, execLDR)
    }
    // LDR Rd, [Rn, -Rm, LSR #]!
    def LDROffsetLSRPreDecWB(instruction: Int): Unit = {
      ldrPreDecWB(offsetLSR, execLDR)
    }
    // LDR Rd, [Rn, -Rm, ASR #]!
    def LDROffsetASRPreDecWB(instruction: Int): Unit = {
      ldrPreDecWB(offsetASR, execLDR)
    }
    // LDR Rd, [Rn, -Rm, ROR #]!
    def LDROffsetRORPreDecWB(instruction: Int): Unit = {
      ldrPreDecWB(offsetROR, execLDR)
    }

    // STR Rd, [Rn, Rm, LSL #]
    def STROffsetLSLPreInc(instruction: Int): Unit = {
      strPreInc(offsetLSL, execSTR)
    }
    // STR Rd, [Rn, Rm, LSR #]
    def STROffsetLSRPreInc(instruction: Int): Unit = {
      strPreInc(offsetLSR, execSTR)
    }
    // STR Rd, [Rn, Rm, ASR #]
    def STROffsetASRPreInc(instruction: Int): Unit = {
      strPreInc(offsetASR, execSTR)
    }
    // STR Rd, [Rn, Rm, ROR #]
    def STROffsetRORPreInc(instruction: Int): Unit = {
      strPreInc(offsetROR, execSTR)
    }

    // LDR Rd, [Rn, Rm, LSL #]
    def LDROffsetLSLPreInc(instruction: Int): Unit = {
      ldrPreInc(offsetLSL, execLDR)
    }
    // LDR Rd, [Rn, Rm, LSR #]
    def LDROffsetLSRPreInc(instruction: Int): Unit = {
      ldrPreInc(offsetLSR, execLDR)
    }

    // LDR Rd, [Rn, Rm, ASR #]
    def LDROffsetASRPreInc(instruction: Int): Unit = {
      ldrPreInc(offsetASR, execLDR)
    }

    // LDR Rd, [Rn, Rm, ROR #]
    def LDROffsetRORPreInc(instruction: Int): Unit = {
      ldrPreInc(offsetROR, execLDR)
    }

    // STR Rd, [Rn, Rm, LSL #]!
    def STROffsetLSLPreIncWB(instruction: Int): Unit = {
      strPreIncWB(offsetLSL, execSTR)
    }
    // STR Rd, [Rn, Rm, LSR #]!
    def STROffsetLSRPreIncWB(instruction: Int): Unit = {
      strPreIncWB(offsetLSR, execSTR)
    }

    // STR Rd, [Rn, Rm, ASR #]!
    def STROffsetASRPreIncWB(instruction: Int): Unit = {
      strPreIncWB(offsetASR, execSTR)
    }

    // STR Rd, [Rn, Rm, ROR #]!
    def STROffsetRORPreIncWB(instruction: Int): Unit = {
      strPreIncWB(offsetROR, execSTR)
    }

    // LDR Rd, [Rn, Rm, LSL #]!
    def LDROffsetLSLPreIncWB(instruction: Int): Unit = {
      ldrPreIncWB(offsetLSL, execLDR)
    }

    // LDR Rd, [Rn, Rm, LSR #]!
    def LDROffsetLSRPreIncWB(instruction: Int): Unit = {
      ldrPreIncWB(offsetLSR, execLDR)
    }

    // LDR Rd, [Rn, Rm, ASR #]!
    def LDROffsetASRPreIncWB(instruction: Int): Unit = {
      ldrPreIncWB(offsetASR, execLDR)
    }

    // LDR Rd, [Rn, Rm, ROR #]!
    def LDROffsetRORPreIncWB(instruction: Int): Unit = {
      ldrPreIncWB(offsetROR, execLDR)
    }
  }

  // Branch
  def bMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    var offset = instruction.value & 0x00ffffff
    c.Expr(
      q"""
        if(($offset & 0x00800000) == 1) $offset |= 0xff000000

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
        if(($offset & 0x00800000) == 1) $offset |= 0xff000000

        ${em.registers(14)} = ${em.registers(15)} - 4
        ${em.registers(15)} = $offset << 2
        ${em.nextPC} = ${em.registers(15)}
        ${em.registers(15)} += 4
        ${em.armPrefetch()}
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

  def armExecuteMacro(instruction: Int): Int = {
    // Only twelve bits needed to decode
    // bits[27:20] (inclusive)
//    val identifier = (instruction >> 20) & 0x0ff
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
        case 1 => if(op == 1) decodeMedia(instruction) else decodeLoadStore(instruction)
        //branch ops
        case 2 => decodeBranch(instruction)
        case 3 => decodeCoProcessing(instruction)
        case _ => null
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
    val stateChange = op & 1
    ALU.init(instruction)

    op match {
      case 0 => ALU.and()
      case 2 => ALU.sub()
      case 4 => ALU.add()
    }
  }

  //offset and addressing calls
  def decodeLoadStore(instruction: Int): Unit = {
    val op = (instruction >> 20) & 0x0ff

    // Determine value of L bit
    // 1 = load, 0 = store
    if((op & 1) == 1) {
      if((op & 4) == 4) { // xx1x1
        if((op == 7) || (op == 15)) { // 0x111, LDRBT

        } else { // LDRB

        }
      } else { // xx0x1
        if((op == 3) || (op == 1)) { // 0x011 ldrt

        } else { // ldr
          loadStore.ldr()
        }
      }
    } else {
      if((op & 4) == 4) {
        if((op == 6) || (op == 14)) { // strbt

        } else { //strb

        }
      } else {
        if((op == 2) || (op == 10)) { //strt

        } else { //str
          loadStore.str()
        }
      }
    }
  }

  def decodeMultiply(instruction: Int): Unit = {

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
