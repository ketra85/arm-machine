//This looks like it will work if levels are implemented but will be removed for now
// conditionals: Boolean, extraBits: Boolean
case class CMN(val name: String,
               val conditionals: Boolean,
               val extraBits: Boolean) extends Instruction {


  def operation(rd: String, rn: String, shift: String): Boolean = {
    //    rd = rn + shift
    rn == rd
  }
}
