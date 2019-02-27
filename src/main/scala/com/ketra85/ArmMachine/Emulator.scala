package com.ketra85.ArmMachine

// Emulator class encapsulating emulator operations
// Memory, Registers, etc
class Emulator(memorySize: Int, numberOfRegisters: Int) {
  private val DEFAULT_MEMORY_SIZE = 65536
  private val DEFAULT_NUMBER_OF_GENERAL_REGISTERS = 15
  private val DEFAULT_PC_REGISTERS = 1
  private val DEFAULT_FLAG_REGISTERS = 2

  // A general, program counter, and flag registers
  private val MIN_REGISTERS = 3

  // rewrite this
  // a gp, pc, and flag register is always needed
  // r15 pc
  // r14 link register
  // SPSR which is a copy of the flag register (CPSR)
  val registers = numberOfRegisters match {
    case x if x <= 3 =>
      for(x <- DEFAULT_NUMBER_OF_GENERAL_REGISTERS)
        yield new Register("r" + x, RegTypeEnums.generalUnbanked)
      for(x <- DEFAULT_PC_REGISTERS)
        yield new Register("r" + x, RegTypeEnums.pc)
      for(x <- DEFAULT_FLAG_REGISTERS)
        yield new Register("r" + x, RegTypeEnums.flag)
    case _ => numberOfRegisters
  }

//  def this(memorySize: Int) {
//    this(memorySize, DEFAULT_NUMBER_OF_REGISTERS)
//  }
//
//  def this(numberOfRegisters : Int) {
//    this(DEFAULT_MEMORY_SIZE, numberOfRegisters)
//  }

}
