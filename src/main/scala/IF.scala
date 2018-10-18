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

      val target = Input(UInt(32.W))
      val branch = Input(Bool())

      val out = Output(new IFBarrier.Contents)

      // setup/test
      val IMEMsetup = Input(new IMEMsetupSignals)
      val PC = Output(UInt(32.W))
    })

  // Setup
  val IMEM = Module(new IMEM).io
  val PC = RegInit(UInt(32.W), 0.U)
  IMEM.setup := io.IMEMsetup

  IMEM.instructionAddress := PC
  val instruction = Wire(new Instruction)
  instruction := IMEM.instruction.asTypeOf(new Instruction)

  /**
    Your code here
   */
  PC := Mux(io.branch, io.target, PC + 4.U)

  when(io.IMEMsetup.setup) {
    PC := 0.U
  }

  val PC_out = RegNext(PC)
  io.PC := PC
  io.out.PC := PC
  io.out.insn := instruction
}
