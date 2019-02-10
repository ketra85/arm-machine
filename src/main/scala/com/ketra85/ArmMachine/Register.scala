package com.ketra85.ArmMachine
import com.ketra85.ArmMachine.RegTypeEnums.RegType

class Register(
                val name: String,
                val regType: RegType) {

  var value = regType match {
    case RegTypeEnums.general => 0
    case RegTypeEnums.pc => 0
    // what should this be?
    case RegTypeEnums.flag => {
      var N = false
      var Z = false
      var C = false
      var V = false
    }
  }

  // The below methods in case of flag?
  //explore defining the registers in the above match statement
//  def read(): Int = {
//    value
//  }
//
//  def write(value: Int): Unit = {
//    this.value = value
//  }

}

object RegTypeEnums {
  sealed trait RegType
  case object general extends RegType
  case object pc extends RegType
  case object flag extends RegType

  val Conditionals = Seq(general, pc, flag)
}