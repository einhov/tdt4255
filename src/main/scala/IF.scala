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

  IMEM.instructionAddress := PC
  val instruction = Wire(new Instruction)
  instruction := IMEM.instruction.asTypeOf(new Instruction)

  // Update PC
  PC := MuxCase(PC + 4.U, Array(
    io.freeze -> PC,
    io.branch -> io.target
  ))

  when(io.IMEMsetup.setup) {
    PC := 0.U
    instruction := Instruction.default
  }

  io.PC := PC
  io.out.PC := PC
  io.out.insn := instruction
}
