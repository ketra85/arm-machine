package com.ketra85.ArmMachine

// reg 16 -> R13_SVC
// reg 17 -> R14_SVC
// reg 18 -> R13_abt
// reg 19 -> R14_abt
// reg 20 -> R13_und
// reg 21 -> R14_und
// reg 22 -> R13_irq
// reg 23 -> R14_irq
// reg 24 -> R8_fig
// reg 25 -> R9_fig
// reg 26 -> R10_fig
// reg 27 -> R11_fig
// reg 28 -> R12_fig
// reg 29 -> R13_fig
// reg 30 -> R14_fig
package object Globals {
  val R13_SVC = 16
  val R14_SVC = 17
  val R13_ABT = 18
  val R14_ABT = 19
  val R13_UND = 20
  val R14_UND = 21
  val R13_IRQ = 22
  val R14_IRQ = 23
  val R8_FIQ = 24
  val R9_FIQ = 25
  val R10_FIQ = 26
  val R11_FIQ = 27
  val R12_FIQ = 28
  val R13_FIQ = 29
  val R14_FIQ = 30
}
