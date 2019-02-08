package com.ketra85.ArmMachine

class Level(val winState: Emulator) {
//  val winState: Emulator
//  def this() {
//    // Win state equal to paramters for emulator set within the xml or dll file
//  }


  // Checks if the current state mirrors the required parameters
  def winCondition(emulator: Emulator): Boolean = {
    true
  }

//  def winCondition(answer: String): Boolean = {
//
//  }

  def levelDialogue(dialogue: String): String = {
    dialogue
  }

}
