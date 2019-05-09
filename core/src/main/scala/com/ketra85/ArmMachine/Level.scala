package com.ketra85.ArmMachine

class Level(val winState: Emulator, val question: String) {
//  val winState: Emulator
//  def this() {
//    // Win state equal to paramters for emulator set within the xml or dll file
//  }
  def this(winState: Emulator) {
    this(winState, "")
  }

  //Unsure of this implementation
  def this(question: String) {
    this(Class[Emulator],question)
  }

  // Checks if the current state mirrors the required parameters
  def winCondition(emulator: Emulator): Boolean = {
    true
  }

  def winCondition(answer: String): Boolean = {
    true
  }

  def levelDialogue(dialogue: String): String = {
    dialogue
  }

}
