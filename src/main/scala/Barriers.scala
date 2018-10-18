package Ov1

import chisel3._

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
    }
  )

  val PC_delay = RegNext(io.in.PC)

  io.out.insn := io.in.insn
  io.out.PC := PC_delay
}

object IDBarrier {
  class Contents extends Bundle {
    val PC = UInt(32.W)
    val controlSignals = new ControlSignals
    val branchType = UInt(3.W)
    val rs1 = UInt(32.W)
    val rs2 = UInt(32.W)
    val op1Sel = UInt(1.W)
    val op2Sel = UInt(1.W)
    val immediate = UInt(32.W)
    val rd = UInt(5.W)
    val ALUop = UInt(4.W)
  }
}

class IDBarrier extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new IDBarrier.Contents)
      val out = Output(new IDBarrier.Contents)
    }
  )

  val contents = Reg(new IDBarrier.Contents)
  contents <> io.in
  io.out <> contents
}

object EXBarrier {
  class Contents extends Bundle {
    val PC = UInt(32.W)
    val address = UInt(32.W)
    val read = Bool()
    val write = Bool()

    val rd = UInt(5.W)
    val wb = Bool()
    val data = UInt(32.W)
  }
}

class EXBarrier extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new EXBarrier.Contents)
      val out = Output(new EXBarrier.Contents)
    }
  )

  val contents = Reg(new EXBarrier.Contents)
  contents <> io.in
  io.out <> contents
}

object MEMBarrier {
  class Contents extends Bundle {
    val PC = UInt(32.W)
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
}
