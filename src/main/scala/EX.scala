package Ov1
import chisel3._
import chisel3.util._

class Execute extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new IDBarrier.Contents)
      val out = Output(new EXBarrier.Contents)

      val target = Output(UInt(32.W))
      val branch = Output(Bool())
    }
  )

  val op1 = Mux(io.in.op1Sel === Op1Select.rs1, io.in.rs1, io.in.PC)
  val op2 = Mux(io.in.op2Sel === Op2Select.rs2, io.in.rs2, io.in.immediate)
  val op1s = op1.asSInt
  val op2s = op2.asSInt

  io.out.data := 0.U
  val aluResult = WireInit(0.U(32.W))
  switch(io.in.ALUop) {
    is(ALUOps.ADD) { aluResult := op1 + op2 }
    is(ALUOps.SUB) { aluResult := op1 - op2 }
    is(ALUOps.AND) { aluResult := op1 & op2 }
    is(ALUOps.OR)  { aluResult := op1 | op2 }
    is(ALUOps.XOR) { aluResult := op1 ^ op2 }
    is(ALUOps.SLT) { aluResult := op1s < op2s }
    is(ALUOps.SLTU){ aluResult := op1 < op2 }
    is(ALUOps.SLL) { aluResult := op1 << op2(4, 0) }
    is(ALUOps.SRL) { aluResult := op1 >> op2(4, 0) }
    is(ALUOps.SRA) { aluResult := (op1s >> op2(4, 0)).asUInt }
  }
  io.out.data := aluResult

  when(io.in.controlSignals.jump) {
    io.branch := true.B
    io.target := aluResult & (0.U(32.W) - 2.U)
    io.out.data := io.in.PC + 4.U
  } .elsewhen(io.in.controlSignals.branch) {
    val take_branch = WireInit(false.B)
    val rs1 = io.in.rs1
    val rs2 = io.in.rs2
    val rs1s = io.in.rs1.asSInt
    val rs2s = io.in.rs2.asSInt
    switch(io.in.branchType) {
      is(branchType.beq ) { take_branch := rs1  === rs2  }
      is(branchType.gte ) { take_branch := rs1s >=  rs2s }
      is(branchType.gteu) { take_branch := rs1  >=  rs2  }
      is(branchType.lt  ) { take_branch := rs1s <   rs2s }
      is(branchType.ltu ) { take_branch := rs1  <   rs2  }
      is(branchType.neq ) { take_branch := rs1  =/= rs2  }
    }
    io.branch := take_branch
    io.target := aluResult
    io.out.data := 0.U
  } .otherwise {
    io.branch := false.B
    io.target := 0.U
  }

  io.out.address := io.in.immediate
  io.out.read := io.in.controlSignals.memRead
  io.out.write := io.in.controlSignals.memWrite
  io.out.wb := io.in.controlSignals.regWrite
  io.out.rd := io.in.rd

  io.out.PC := io.in.PC
}