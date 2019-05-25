package com.ketra85.ArmMachine


import com.ketra85.ArmMachine.Flag.FlagSet
import Globals._

// Emulator class encapsulating emulator operations
// Memory, Registers, etc


class Emulator() {

  object ProcessorMode extends Enumeration {
    val USR: ProcessorMode.Value = Value(0x10)
    val FIQ: ProcessorMode.Value = Value(0x11)
    val IRQ: ProcessorMode.Value = Value(0x12)
    val SVC: ProcessorMode.Value = Value(0x13)
    val ABT: ProcessorMode.Value = Value(0x17)
    val UND: ProcessorMode.Value = Value(0x1b)
    val SYS: ProcessorMode.Value = Value(0x1f)

  }

//  private val DEFAULT_NUMBER_OF_GENERAL_REGISTERS = 15
//  private val DEFAULT_PC_REGISTERS = 1
//  private val DEFAULT_FLAG_REGISTERS = 2
  var prefetch: List[Int] = List.fill(2)(0)
  var memory: MMU = MMU(this)
  var nextPC: Int = 0x00000000
  var armState: Boolean = true
  var irq: Boolean = true
  var currMode: ProcessorMode.Value = ProcessorMode.SYS

  var registers: List[Int] = List.fill[Int](31)(0)

  var cpsr = Flag.FlagSet()
  var spsr = Flag.FlagSet()
  var spsrSVC = Flag.FlagSet()
  var spsrABT = Flag.FlagSet()
  var spsrUND = Flag.FlagSet()
  var spsrIRQ = Flag.FlagSet()
  var spsrFIQ = Flag.FlagSet()

  var N_FLAG = false
  var C_FLAG = false
  var Z_FLAG = false
  var V_FLAG = false



  def updateCPSR(): Unit = {
    if(N_FLAG) cpsr.|(FlagSet(Flag.N))
    if(C_FLAG) cpsr.|(FlagSet(Flag.C))
    if(Z_FLAG) cpsr.|(FlagSet(Flag.Z))
    if(V_FLAG) cpsr.|(FlagSet(Flag.V))
    if(!armState) cpsr.|(FlagSet(Flag.T))
    if(!irq) cpsr.|(FlagSet(Flag.I))

//    cpsr.|(Flag(currMode))
  }

  def updateFlags(): Unit = {
    N_FLAG = cpsr.isSet(Flag.N)
    C_FLAG = cpsr.isSet(Flag.C)
    Z_FLAG = cpsr.isSet(Flag.Z)
    V_FLAG = cpsr.isSet(Flag.V)
    armState = cpsr.isSet(Flag.T)
    irq = cpsr.isSet(Flag.I)
  }

  def switchMode(mode: ProcessorMode.Value, save: Boolean): Unit = {
    //update flags
    updateCPSR()

    currMode match {
      case ProcessorMode.USR =>
      case ProcessorMode.SYS => registers(9) = 0
      case ProcessorMode.FIQ =>
        registers(R13_FIQ) = registers(13)
        registers(R14_FIQ) = registers(14)
        spsrFIQ = cpsr
      case ProcessorMode.IRQ =>
        registers(R13_IRQ) = registers(13)
        registers(R14_IRQ) = registers(14)
        spsrIRQ = cpsr
      case ProcessorMode.SVC =>
        registers(R13_SVC) = registers(13)
        registers(R14_SVC) = registers(14)
        spsrSVC = cpsr
      case ProcessorMode.ABT =>
        registers(R13_ABT) = registers(13)
        registers(R14_ABT) = registers(14)
        spsrABT = cpsr
      case ProcessorMode.UND =>
        registers(R13_UND) = registers(13)
        registers(R14_UND) = registers(14)
        spsrUND = cpsr
    }

    mode match {
      case ProcessorMode.USR =>
      case ProcessorMode.SYS => registers(9) = 0
      case ProcessorMode.FIQ =>
        // need register swap method
        registers(13) = registers(R13_FIQ)
        registers(14) = registers(R14_FIQ)
        if (save) spsr = cpsr else spsr = spsrFIQ;
      case ProcessorMode.IRQ =>
        registers(13) = registers(R13_IRQ)
        registers(14) = registers(R14_IRQ)
        if (save) spsr = cpsr else spsr = spsrIRQ;
      case ProcessorMode.SVC =>
        registers(13) = registers(R13_SVC)
        registers(14) = registers(R14_SVC)
        if (save) spsr = cpsr else spsr = spsrSVC;
      case ProcessorMode.ABT =>
        registers(13) = registers(R13_ABT)
        registers(14) = registers(R14_ABT)
        if (save) spsr = cpsr else spsr = spsrABT;
      case ProcessorMode.UND =>
        registers(13) = registers(R13_UND)
        registers(14) = registers(R14_UND)
        if (save) spsr = cpsr else spsr = spsrUND;
    }

    currMode = mode
    updateFlags()
    updateCPSR()
  }

  def undefinedException(): Unit = {
    val pc = registers(15)
    val savedState = armState
    //switch mode
    registers(14) = pc - (if(savedState) 4 else 2)
    registers(15) = 0x04
    armState = true
    irq = false
    nextPC = 0x04
    armPrefetch()
    registers(15) += 4
  }

  def softwareInterrupt(): Unit = {
    val savedState = armState
    //switch mode
    switchMode(ProcessorMode.FIQ, save = true)
    registers(14) = registers(15) - (if(savedState) 4 else 2)
    registers(15) = 0x08
    armState = true
    irq = false
    nextPC = 0x08
    armPrefetch()
    registers(15) += 4
  }

  def interrupt(): Unit = {
    val savedState = armState
    // switch modes
    switchMode(ProcessorMode.IRQ, save = true)
    registers(14) = registers(15)
    if(!savedState) registers(14) += 2
    registers(15) = 0x18
    armState = true
    irq = false
    nextPC += 4
    armPrefetch()
  }

  def armPrefetch(): Unit = {
    prefetch(0) = memory.read32(nextPC)
    prefetch(1) = memory.read32(nextPC + 4)
  }

  def armPrefetchNext(): Unit = {
    prefetch(1) = memory.read32(nextPC + 4)
  }

  def thumbPrefetch(): Unit = {
    prefetch(0) = memory.read16(nextPC)
    prefetch(1) = memory.read16(nextPC + 2)
  }

  def thumbPrefetchNext(): Unit = {
    prefetch(1) = memory.read16(nextPC + 2)
  }
}
