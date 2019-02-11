package com.ketra85.ArmMachine

// Emulator class encapsulating emulator operations
// Memory, Registers, etc
class Emulator(memorySize: Int, numberOfRegisters: Int) {
  val DEFAULT_MEMORY_SIZE = 65536
  val DEFAULT_NUMBER_OF_GENERAL_REGISTERS = 15
  val DEFAULT_PC_REGISTERS = 1
  val DEFAULT_FLAG_REGISTERS = 2

  // A general, program counter, and flag registers
  val MIN_REGISTERS = 3

  // rewrite this
  // a gp, pc, and flag register is always needed
  // r15 pc
  // r14 link register
  // SPSR which is a copy of the flag register (CPSR)
  val registers = numberOfRegisters match {
    case x if x <= 3 =>
      for(x <- DEFAULT_NUMBER_OF_GENERAL_REGISTERS)
        yield new Register("r" + x, RegTypeEnums.general)
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

  object Memory {
    var memory = memorySize match {
      case x if x <= 0 => new Array[Int](DEFAULT_MEMORY_SIZE)
      case _ => new Array[Int](memorySize)
    }

    // Multiple implementations, supporting shift and indexed addressing
    def read(address: Int) : Int = {
      memory(address)
    }

    // Same as read method
    def write(address : Int, value : Int) : Unit = {
      memory(address) = value
    }
  }

}
