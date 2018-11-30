package Ov1
import chisel3._
import chisel3.util._

class Execute extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new IDBarrier.Contents)
      val out = Output(new EXBarrier.Contents)
      val forward_ex = Input(new RegisterForwardSignals)
      val forward_mem = Input(new RegisterForwardSignals)

      val branchTaken = Output(Bool())
      val updateBranchTaken = Output(Bool())
    }
  )

  val rv1 = MuxCase(io.in.rv1, Array(
    (io.forward_ex.valid() && (io.in.rs1 === io.forward_ex.operand)) -> io.forward_ex.value,
    (io.forward_mem.valid() && (io.in.rs1 === io.forward_mem.operand)) -> io.forward_mem.value,
  ))
  val rv2 = MuxCase(io.in.rv2, Array(
    (io.forward_ex.valid() && (io.in.rs2 === io.forward_ex.operand)) -> io.forward_ex.value,
    (io.forward_mem.valid() && (io.in.rs2 === io.forward_mem.operand)) -> io.forward_mem.value,
  ))

  val op1 = Mux(io.in.op1Sel === Op1Select.rs1, rv1, io.in.PC)
  val op2 = Mux(io.in.op2Sel === Op2Select.rs2, rv2, io.in.immediate)
  val op1s = op1.asSInt
  val op2s = op2.asSInt

  val aluResult = WireInit(0.U(32.W))
  switch(io.in.ALUop) {
    is(ALUOps.ADD)    { aluResult := op1 + op2 }
    is(ALUOps.SUB)    { aluResult := op1 - op2 }
    is(ALUOps.AND)    { aluResult := op1 & op2 }
    is(ALUOps.OR)     { aluResult := op1 | op2 }
    is(ALUOps.XOR)    { aluResult := op1 ^ op2 }
    is(ALUOps.SLT)    { aluResult := op1s < op2s }
    is(ALUOps.SLTU)   { aluResult := op1 < op2 }
    is(ALUOps.SLL)    { aluResult := op1 << op2(4, 0) }
    is(ALUOps.SRL)    { aluResult := op1 >> op2(4, 0) }
    is(ALUOps.SRA)    { aluResult := (op1s >> op2(4, 0)).asUInt }
    is(ALUOps.COPY_B) { aluResult := op2 }
  }
  io.out.data := aluResult

  io.updateBranchTaken := false.B
  io.branchTaken := DontCare
  when(io.in.controlSignals.jump) {
    io.out.branch := true.B
    io.out.target := aluResult & (0.U(32.W) - 2.U)
    io.out.data := io.in.PC + 4.U
  } .elsewhen(io.in.controlSignals.branch) {
    val take_branch = WireInit(false.B)
    val rv1s = rv1.asSInt
    val rv2s = rv2.asSInt
    switch(io.in.branchType) {
      is(branchType.beq ) { take_branch := rv1  === rv2  }
      is(branchType.gte ) { take_branch := rv1s >=  rv2s }
      is(branchType.gteu) { take_branch := rv1  >=  rv2  }
      is(branchType.lt  ) { take_branch := rv1s <   rv2s }
      is(branchType.ltu ) { take_branch := rv1  <   rv2  }
      is(branchType.neq ) { take_branch := rv1  =/= rv2  }
    }

    io.out.branch := false.B
    io.out.target := 0.U
    io.out.data := 0.U
    when(take_branch) {
      when(!io.in.branchEarly) {
        io.out.branch := true.B
        io.out.target := aluResult
      }
      io.branchTaken := true.B
      io.updateBranchTaken := true.B
    } .otherwise {
      when(io.in.branchEarly) {
        io.out.branch := true.B
        io.out.target := io.in.PC + 4.U
      }
      io.branchTaken := false.B
      io.updateBranchTaken := true.B
    }
  } .otherwise {
    io.out.branch := false.B
    io.out.target := 0.U
  }

  io.out.address := aluResult
  io.out.read := io.in.controlSignals.memRead
  io.out.write := io.in.controlSignals.memWrite
  io.out.write_data := rv2
  io.out.wb := io.in.controlSignals.regWrite
  io.out.rd := io.in.rd

  io.out.PC := io.in.PC

  io.out.forward.value := aluResult
  io.out.forward.operand := Mux(io.in.controlSignals.regWrite, io.in.rd, 0.U)
}
