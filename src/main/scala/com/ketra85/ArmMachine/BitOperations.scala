package com.ketra85.ArmMachine

object BitOperations {

  // Why does royta use a 9 digit hex? Am I missing something?
  def xor(x: Int, y: Int): Int = {
    val result = x ^ y
    if (result >= 0) return result else return result + 0x10000000
  }

  def or(x: It): Unit = {

  }

  def and(): Unit = {

  }

  def not(): Unit = {

  }

}
