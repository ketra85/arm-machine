import com.ketra85.ArmMachine.Emulator

trait Instruction {
  def name: String
  def conditionals: Boolean
  def extraBits: Boolean

//  def operation(rd: String, rn: String, shift: String): Boolean
//  def operation(rd: String, shift: String)

//  def operation()
}
