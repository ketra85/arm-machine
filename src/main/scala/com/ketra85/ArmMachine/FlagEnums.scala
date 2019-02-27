package com.ketra85.ArmMachine
import scala.collection.BitSet

object FlagEnums extends Enumeration {
  protected case class Val(name: String, val mask: Int) extends super.Val(nextId, name)

  type FlagEnums = Val

  val N = Val("N", 1)
  val Z = Val("Z", 2)
  val C = Val("C", 4)
  val V = Val("V", 8)

  case class FlagSet(bits: BitSet) {
    def isSet(flag: FlagEnums) = bits.contains(flag.mask)
    def +(flag: FlagEnums) = new FlagSet(bits + flag.mask)
    def -(flag: FlagEnums) = new FlagSet(bits - flag.mask)
    def &(other: FlagSet) = new FlagSet(bits & other.bits)
    def &~(other: FlagSet) = new FlagSet(bits &~ other.bits)
    def ^(other: FlagSet) = new FlagSet(bits ^ other.bits)
    def |(other: FlagSet) = new FlagSet(bits | other.bits)
    def size = bits.size
  }

  object FlagSet {
    def apply(flags: FlagEnums*): FlagSet = apply(BitSet(flags.map(_.mask):_*))
    def apply(flags: ValueSet): FlagSet = apply(BitSet(flags.toSeq.map{ case m: FlagEnums => m.mask}:_*))
  }
}
