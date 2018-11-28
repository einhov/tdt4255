package Ov1
import chisel3._
import chisel3.util._

class InstructionFetch extends Module {
  val io = IO(
    new Bundle {
      val target = Input(UInt(32.W))
      val branch = Input(Bool())

      val out = Output(new IFBarrier.Contents)

      val freeze = Input(Bool())

      // setup/test
      val IMEMsetup = Input(new IMEMsetupSignals)
      val PC = Output(UInt(32.W))
    })

  // Setup
  val IMEM = Module(new IMEM).io
  val PC = RegInit(UInt(32.W), 0.U)
  IMEM.setup := io.IMEMsetup

  val target_pc = Mux(!io.branch, PC, io.target)
  val next_pc = target_pc + 4.U

  IMEM.instructionAddress := target_pc
  val instruction = Wire(new Instruction)
  instruction := IMEM.instruction.asTypeOf(new Instruction)

  PC := Mux(!io.freeze, next_pc, PC)

  when(io.IMEMsetup.setup) {
    PC := 0.U
    instruction := Instruction.default
  }

  io.PC := target_pc
  io.out.PC := target_pc
  io.out.insn := instruction
}
