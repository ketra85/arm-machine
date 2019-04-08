import com.ketra85.ArmMachine.Conditionals.Cond
import com.ketra85.ArmMachine.{RegTypeEnums, Register}

//This looks like it will work if levels are implemented but will be removed for now
// conditionals: Boolean, extraBits: Boolean
case class ADD(val name: String,
          val conditionals: Boolean,
          val extraBits: Boolean) extends Instruction {


  def operation(rd: Register, rn: Register, shift: Int,
                condition: Boolean, sBit: Boolean, regs: Register*) {
//    rd = rn + shift
    if (condition) {
      rd.value = rn.value + shift
      if(sBit && rd.regType == RegTypeEnums.pc) {
        // Also set CPSR = SPSR flags

      } else if(sBit) {
        // Set flags
      }
    }
  }
}
