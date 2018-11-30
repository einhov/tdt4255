package Ov1

import chisel3._
import chisel3.util._

object IFBarrier {
  class Contents extends Bundle {
    val PC = UInt(32.W)
    val insn = new Instruction
  }
}

class IFBarrier extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new IFBarrier.Contents)
      val out = Output(new IFBarrier.Contents)
      val freeze = Input(Bool())
      val nop = Input(Bool())
    }
  )

  // Instruction holding during freeze
  val insn_held = RegInit(false.B)
  val insn_hold = Reg(new Instruction)
  when(io.freeze && !insn_held) {
    insn_hold := io.in.insn
    insn_held := true.B
  }

  when(!io.freeze && insn_held) {
    insn_held := false.B
  }

  val PC_delay = RegEnable(io.in.PC, ~io.freeze)

  io.out.insn := Mux(!insn_held, io.in.insn, insn_hold)
  io.out.PC := PC_delay

  when(io.nop) {
    insn_held := false.B
    io.out.PC := 0.U
    io.out.insn := Instruction.default
  }

}

object IDBarrier {
  class Contents extends Bundle {
    val PC = UInt(32.W)
    val controlSignals = new ControlSignals
    val branchType = UInt(3.W)
    val rs1 = UInt(4.W)
    val rs2 = UInt(4.W)
    val rv1 = UInt(32.W)
    val rv2 = UInt(32.W)
    val op1Sel = UInt(1.W)
    val op2Sel = UInt(1.W)
    val immediate = UInt(32.W)
    val rd = UInt(5.W)
    val ALUop = UInt(4.W)

    val branchEarly = Bool()
    val target = Bool()
  }
}

class IDBarrier extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new IDBarrier.Contents)
      val out = Output(new IDBarrier.Contents)
      val freeze = Input(Bool())
      val nop = Input(Bool())
    }
  )

  val nop = Wire(new IDBarrier.Contents)
  nop.PC := 0.U
  nop.controlSignals := ControlSignals.nop
  nop.branchType := 0.U
  nop.rs1 := 0.U
  nop.rs2 := 0.U
  nop.rv1 := 0.U
  nop.rv2 := 0.U
  nop.op1Sel := 0.U
  nop.op2Sel := 0.U
  nop.immediate := 0.U
  nop.rd := 0.U
  nop.ALUop := 0.U
  nop.branchEarly := false.B
  nop.target := DontCare

  val contents = Reg(new IDBarrier.Contents)
  when(!io.freeze) {
    contents := Mux(!io.nop, io.in, nop)
  }
  io.out := Mux(!io.nop, contents, nop)
}

object EXBarrier {
  class Contents extends Bundle {
    val PC = UInt(32.W)
    val address = UInt(32.W)
    val read = Bool()
    val write = Bool()
    val write_data = UInt(32.W)

    val rd = UInt(5.W)
    val wb = Bool()
    val data = UInt(32.W)

    val branch = Bool()
    val target = UInt(32.W)

    val forward = new RegisterForwardSignals
  }
}

class EXBarrier extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new EXBarrier.Contents)
      val out = Output(new EXBarrier.Contents)
      val nop = Input(Bool())
    }
  )

  val nop = Wire(new EXBarrier.Contents)
  nop.PC := 0.U
  nop.address := 0.U
  nop.read := false.B
  nop.write := false.B
  nop.write_data := 0.U
  nop.rd := 0.U
  nop.wb := false.B
  nop.data := 0.U
  nop.branch := false.B
  nop.target := 0.U
  nop.forward.operand := 0.U
  nop.forward.value := 0.U

  val contents = RegInit(new EXBarrier.Contents, nop)

  contents := Mux(!io.nop, io.in, nop)
  io.out <> contents
}

object MEMBarrier {
  class Contents extends Bundle {
    val PC = UInt(32.W)
    val rd = UInt(5.W)
    val wb = Bool()
    val register_data = UInt(32.W)
    val memory_data = UInt(32.W)
    val dataIsFromMem = Bool()

    val forward = new RegisterForwardSignals
  }
}

class MEMBarrier extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new MEMBarrier.Contents)
      val out = Output(new MEMBarrier.Contents)
    }
  )

  val contents = Reg(new MEMBarrier.Contents)
  contents <> io.in
  io.out <> contents
  io.out.memory_data := io.in.memory_data
}
