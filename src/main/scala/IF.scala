package Ov1
import chisel3._

class InstructionFetch extends Module {
  val io = IO(
    new Bundle {

      /**
        You need to add inputs and outputs here
        A good start is branch/jump address as input, and
        instruction as output.
        */


      // setup/test
      val IMEMsetup = Input(new IMEMsetupSignals)
    })

  // Setup
  val IMEM = Module(new IMEM).io
  val PC = RegInit(UInt(32.W), 0.U)
  IMEM.setup := io.IMEMsetup
  io.PC := PC

  IMEM.instructionAddress := PC
  val instruction = Wire(new Instruction)
  instruction := IMEM.instruction.asTypeOf(new Instruction)
}
