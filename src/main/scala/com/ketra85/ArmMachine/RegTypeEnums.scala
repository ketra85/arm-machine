package com.ketra85.ArmMachine

object RegTypeEnums {
  sealed trait RegType
  case object generalUnbanked extends RegType
  case object generalBanked extends RegType
  case object link extends RegType
  case object stackPointer extends RegType
  case object pc extends RegType
  case object flag extends RegType

  val Conditionals = Seq(generalUnbanked, generalBanked, link, stackPointer, pc, flag)
}
