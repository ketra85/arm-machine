package com.ketra85.ArmMachine
import com.ketra85.ArmMachine.RegTypeEnums.RegType

class Register(name: String, val regType: RegType) {

//  var value = regType match {
//    case RegTypeEnums.generalUnbanked || RegTypeEnums.generalBanked || RegTypeEnums.pc => 0
//    // what should this be?
//    case RegTypeEnums.flag => {
//      var N = false
//      var Z = false
//      var C = false
//      var V = false
//    }
//  }

  var value = 0

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

