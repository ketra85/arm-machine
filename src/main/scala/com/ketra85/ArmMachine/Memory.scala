package com.ketra85.ArmMachine


case class Memory(memorySize: Int)

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
