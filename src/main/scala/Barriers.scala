package Ov1

import chisel3._

class IFBarrier extends Module {
  val io = IO(
    new Bundle {
      val PC_in = Input(UInt(32.W))
      val insn_in = Input(new Instruction)

      val PC = Output(UInt(32.W))
      val insn = Output(new Instruction)
    }
  )

  val PC_delay = RegNext(io.PC_in)

  io.insn := io.insn_in
  io.PC := PC_delay
}

class IDBarrier extends Module {
  val io = IO(
    new Bundle {

    }
  )

}

class EXBarrier extends Module {
  val io = IO(
    new Bundle {

    }
  )

}

class MEMBarrier extends Module {
  val io = IO(
    new Bundle {

    }
  )

}
