package com.ketra85.ArmMachine

case class Memory(emulator: Emulator, memorySize: Int) {

}

//object Memory {
//  private val DEFAULT_MEMORY_SIZE = 65536
//
//  var memory = memorySize match {
//    case x if x <= 0 => new Array[Int](DEFAULT_MEMORY_SIZE)
//    case _ => new Array[Int](memorySize)
//  }
//
//  // Multiple implementations, supporting shift and indexed addressing
//  def read(address: Int) : Int = {
//    memory(address)
//  }
//
//  // Same as read method
//  def write(address : Int, value : Int) : Unit = {
//    memory(address) = value
//  }
//}
