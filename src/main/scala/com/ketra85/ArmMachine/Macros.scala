package com.ketra85.ArmMachine

import scala.reflect.macros.blackbox.Context
import com.ketra85.ArmMachine.Emulator

class Macros(opcode: Int) {

  def adcImm(): Unit = macro adcImmMacro

  def adcImmMacro(c: Context)(): Unit = {
    //      import c.universe._

    val rn = (opcode >>> 16) & 0xf
    val rd = (opcode >>> 12) & 0xf
    val sBit = opcode & 0x00100000
    val imm12 = opcode & 0xfff
    val result = registers(rn) + imm12 + cpsr("c")

    registers(rd) = result

  }
}
