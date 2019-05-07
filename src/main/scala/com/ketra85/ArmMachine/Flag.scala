package com.ketra85.ArmMachine
import scala.collection.BitSet

object Flag extends Enumeration {
  protected case class Val(name: String, val mask: Int) extends super.Val(nextId, name)

  type Flag = Val

  val N = Val("N", 1)
  val Z = Val("Z", 2)
  val C = Val("C", 4)
  val V = Val("V", 8)
  val I = Val("I", 16)
  val F = Val("F", 32)
  val T = Val("T", 64)
  val M = Val("M", 128)

  case class FlagSet(bits: BitSet) {
    def isSet(flag: Flag) = bits.contains(flag.mask)
    def +(flag: Flag) = new FlagSet(bits + flag.mask)
    def -(flag: Flag) = new FlagSet(bits - flag.mask)
    def &(other: FlagSet) = new FlagSet(bits & other.bits)
    def &~(other: FlagSet) = new FlagSet(bits &~ other.bits)
    def ^(other: FlagSet) = new FlagSet(bits ^ other.bits)
    def |(other: FlagSet) = new FlagSet(bits | other.bits)
    def size = bits.size
  }

  object FlagSet {
    def apply(flags: Flag*): FlagSet = apply(BitSet(flags.map(_.mask):_*))
    def apply(flags: ValueSet): FlagSet = apply(BitSet(flags.toSeq.map{ case m: Flag => m.mask}:_*))
  }
}
