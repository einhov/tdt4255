package Ov1
import chisel3._
import chisel3.util.{ Fill, Cat }

/**
  */
class InstructionDecode extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new IFBarrier.Contents)
      val out = Output(new IDBarrier.Contents)

      val rd = Input(UInt(5.W))
      val wb = Input(Bool())
      val data = Input(UInt(32.W))

      val forward = Input(new RegisterForwardSignals)
      val branchPred = Input(Bool())

      // setup/test
      val registerSetup     = Input(new RegisterSetupSignals)
      val registerPeek      = Output(UInt(32.W))

      val testUpdates       = Output(new RegisterUpdates)
    })

  val registers    = Module(new Registers).io
  val control      = Module(new Control).io

  // Wire up register setup
  registers.setup := io.registerSetup
  io.registerPeek := registers.readData1
  io.testUpdates  := registers.testUpdates

  // Make it compile
  registers.writeEnable := io.wb
  registers.writeData := io.data
  registers.writeAddress := io.rd

  control.instruction := io.in.insn

  val insn_bits = io.in.insn.instruction
  val rs1 = insn_bits(19, 15)
  val rs2 = insn_bits(24, 20)
  registers.readAddress1 := rs1
  registers.readAddress2 := rs2

  val immediate = Wire(UInt(32.W))
  when(control.immType === ImmFormat.ITYPE) {
    immediate := Cat(Fill(21, insn_bits(31)), insn_bits(30, 20))
  } .elsewhen(control.immType === ImmFormat.STYPE) {
    immediate := Cat(
      Fill(21, insn_bits(31)),
      insn_bits(30, 25),
      insn_bits(11, 7)
    )
  } .elsewhen(control.immType === ImmFormat.BTYPE) {
    immediate := Cat(
      Fill(20, insn_bits(31)),
      insn_bits(7),
      insn_bits(30, 25),
      insn_bits(11, 8),
      0.U(1.W)
    )
  } .elsewhen(control.immType === ImmFormat.UTYPE) {
    immediate := Cat(insn_bits(31, 12), 0.U(12.W))
  } .elsewhen(control.immType === ImmFormat.JTYPE) {
    immediate := Cat(
      Fill(12, insn_bits(31)),
      insn_bits(19, 12),
      insn_bits(20),
      insn_bits(30, 21),
      0.U(1.W)
    )
  } .elsewhen(control.immType === ImmFormat.SHAMT) {
    immediate := insn_bits(24, 20)
  } .otherwise {
    immediate := 0.U
  }

  val rv1 = Mux(io.forward.valid() && (rs1 === io.forward.operand), io.forward.value, registers.readData1)
  val rv2 = Mux(io.forward.valid() && (rs2 === io.forward.operand), io.forward.value, registers.readData2)
  io.out.controlSignals := control.controlSignals
  io.out.branchType := control.branchType
  io.out.ALUop := control.ALUop
  io.out.rs1 := rs1
  io.out.rs2 := rs2
  io.out.rv1 := rv1
  io.out.rv2 := rv2
  io.out.op1Sel := control.op1Select
  io.out.op2Sel := control.op2Select
  io.out.rd := insn_bits(11, 7)
  io.out.immediate := immediate
  io.out.PC := io.in.PC

  when(control.controlSignals.branch && io.branchPred) {
    io.out.branchEarly := true.B
    io.out.target := io.in.PC + immediate
  } .otherwise {
    io.out.branchEarly := false.B
    io.out.target := 0.U
  }
}
