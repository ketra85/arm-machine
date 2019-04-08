import com.ketra85.ArmMachine.Conditionals.Cond
import com.ketra85.ArmMachine.{Emulator, Register}

//This looks like it will work if levels are implemented but will be removed for now
// conditionals: Boolean, extraBits: Boolean
case class ADC(val name: String,
          val conditionals: Boolean,
          val extraBits: Boolean) extends Instruction {

  // rd = rn + shift
  def operation(rd: Register, rn: Register, shift: Int,
                condition: Boolean, sBit: Boolean, regs: Register*) {
//    rd = rn + shift
    rd.value = rn.value + shift
  }
}
