package com.ketra85.InstructionSet

import com.ketra85.ArmMachine._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class ThumbMacros(em: Emulator) {

  // Push and Pop
  def pushReg(instruction: Int, count: Int, address: Int,
              value: Int, reg: Int): Unit = {
    if(instruction & value) {

    }
  }
  def popReg(instruction: Int): Unit = macro popRegMacro
  def pushList(instruction: Int): Unit = macro pushListMacro
  def pushListLR(instruction: Int): Unit = macro pushListLRMacro
  def popList(instruction: Int): Unit = macro popListMacro
  def popListPC(instruction: Int): Unit = macro popListPCMacro


  def cmpRdRs(dest: Int, value: Int): Unit = macro cmpRdRsMacro
  def and(instruction: Int): Unit = macro andMacro

  // ALU operations
  def cmpRdRsMacro(c: Context)(dest: c.Expr[Int], value: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val lhs = em.registers(dest.value)
    val rhs = value.value
    val res = lhs - rhs

    c.Expr(
      q"""
        ${em.Z_FLAG} = if($res == 0) true else false
        ${em.N_FLAG} =
        ${em.V_FLAG} =
        ${em.C_FLAG} =
       """)
  }

  def andMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val dest: Int = instruction.value & 7

    c.Expr(
      q"""
         ${em.registers(dest)} &= ${em.registers((instruction.value >> 3) & 7)}
         ${em.N_FLAG} = if((${em.registers(dest)} & 0x80000000) == 0x80000000) true else false
         ${em.Z_FLAG} = if(${em.registers(dest) == 0}) true else false
       """)
  }

  // Conditional Branching
  def beq(instruction: Int): Unit = macro beqMacro
  def bne(instruction: Int): Unit = macro bneMacro
  def bcs(instruction: Int): Unit = macro bcsMacro
  def bcc(instruction: Int): Unit = macro bccMacro
  def bmi(instruction: Int): Unit = macro bmiMacro
  def bpl(instruction: Int): Unit = macro bplMacro
  def bvs(instruction: Int): Unit = macro bvsMacro
  def bvc(instruction: Int): Unit = macro bvcMacro
  def bhi(instruction: Int): Unit = macro bhiMacro
  def bls(instruction: Int): Unit = macro blsMacro
  def bge(instruction: Int): Unit = macro bgeMacro
  def blt(instruction: Int): Unit = macro bltMacro
  def bgt(instruction: Int): Unit = macro bgtMacro
  def ble(instruction: Int): Unit = macro bltMacro

  def beqMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(${em.Z_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bneMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(!${em.Z_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bcsMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(${em.C_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bccMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(!${em.C_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bmiMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(${em.N_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bplMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(!${em.N_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bvsMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(${em.V_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bvcMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(!${em.V_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bhiMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(${em.C_FLAG} && !${em.Z_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def blsMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(!${em.C_FLAG} || ${em.Z_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bgeMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(${em.N_FLAG} == ${em.V_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bltMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(${em.N_FLAG} != ${em.V_FLAG}) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bgtMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(!${em.N_FLAG} && (${em.N_FLAG} == ${em.V_FLAG})) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }

  def bleMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    c.Expr(
      q"""
         if(${em.Z_FLAG} || (${em.N_FLAG} != ${em.V_FLAG})) {
          ${em.registers(15)} += (${instruction.value} & 0xff).toByte() << 1
          ${em.nextPC} = ${em.registers(15)}
          ${em.registers(15)} += 2
          ${em.thumbPrefetch()}
         }
       """)
  }


  def swi(instruction: Int): Unit = macro swiMacro

  def swiMacro(c: Context)(): c.Expr[Unit] = {
    import c.universe._

    val address: Int = 0
    c.Expr(
      q"""
        ${em.softwareInterrupt()}
       """)
  }

  def b(instruction: Int): Unit = macro bMacro
  def bllForward(instruction: Int): Unit = macro bllForwardMacro
  def bllBackward(instruction: Int): Unit = macro bllBackwardMacro
  def blh(instruction: Int): Unit = macro blhMacro

  def bMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val offset = (instruction.value & 0x3ff) << 1
    c.Expr(
      q"""
        if((${instruction.value} & 0x0400) == 0x0400) offset |= 0xfffff800
        ${em.registers(15)} += $offset
        ${em.nextPC} = ${em.registers(15)}
        ${em.registers(15)} += 2
        ${em.thumbPrefetch()}
       """)
  }

  def bllForwardMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val offset: Int = instruction.value & 0x7FF
    c.Expr(
      q"""
         ${em.registers(14)} = ${em.registers(15)} + (($offset << 12) | 0xff800000)
       """)
  }

  def bllBackwardMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val offset: Int = instruction.value & 0x7FF
    c.Expr(
      q"""
         ${em.registers(15)} = ${em.registers(15)} + ($offset << 12)
       """)
  }

  def blhMacro(c: Context)(instruction: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val offset: Int = instruction.value & 0x7FF
    val temp: Int = em.registers(15) - 2
    c.Expr(
      q"""
         ${em.registers(15)} = (${em.registers(14)} + ($offset << 1)) & 0xfffffffe
         ${em.nextPC} = ${em.registers(15)}
         ${em.registers(15)} += 2
         ${em.registers(14)} = $temp | 1
         ${em.thumbPrefetch()}
       """)
  }


  def thumbExecute(instruction: Int): Unit = {

  }

}
