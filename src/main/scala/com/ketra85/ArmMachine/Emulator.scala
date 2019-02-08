package com.ketra85.ArmMachine

// Emulator class encapsulating emulator operations
// Memory, Registers, etc
class Emulator(memorySize: Int, numberOfRegisters: Int) {
  val DEFAULT_MEMORY_SIZE = 65536
  val DEFAULT_NUMBER_OF_REGISTERS = 16

  // A general, program counter, and flag registers
  val MIN_REGISTERS = 3

  val registers = numberOfRegisters match {
    case x if x <= 3 =>
      for(x <- DEFAULT_NUMBER_OF_REGISTERS)
        yield new Register("r" + x, RegTypeEnums.general)
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

    def read(address: Int) : Int = {
      memory(address)
    }

    def write(address : Int, value : Int) : Unit = {
      memory(address) = value
    }
  }

}
