package Ov1
import chisel3._
import chisel3.util.BitPat
import chisel3.util.ListLookup

/**
  Responsible for setting control signals, from the "classic" set in ControlSignals
  to select signals for the ID operator muxes
 */
class Control() extends Module {

  val io = IO(new Bundle {
                val instruction    = Input(new Instruction)

                val controlSignals = Output(new ControlSignals)
                val branchType     = Output(UInt(3.W))
                val op1Select      = Output(UInt(1.W))
                val op2Select      = Output(UInt(1.W))
                val immType        = Output(UInt(3.W))
                val ALUop          = Output(UInt(4.W))
              })

  import lookup._
  import Op1Select._
  import Op2Select._
  import branchType._
  import ImmFormat._

  val N = 0.asUInt(1.W)
  val Y = 1.asUInt(1.W)

  val opDC = Op1Select.DC
  val btDC = branchType.DC

  /**
    If you want to you can choose to remove everything that isn't a control signal.
    Another alternative is to just temporarily remove what you don't care about and then add it back afterwards.
    */
  val opcodeMap: Array[(BitPat, List[UInt])] = Array(

    // signal      m2r, regW, memR, memW, b, j, bType, Op1Sel, Op2Sel, ImmSelect,       ALUOp
    // Register-register
    ADD    -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.ADD),
    AND    -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.AND),
    OR     -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.OR),
    SLL    -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.SLL),
    SLT    -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.SLT),
    SLTU   -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.SLTU),
    SRA    -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.SRA),
    SRL    -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.SRL),
    SUB    -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.SUB),
    XOR    -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    rs2,    ImmFormat.DC,    ALUOps.XOR),

    // Register-immediate
    ADDI   -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.ITYPE, ALUOps.ADD),
    ANDI   -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.ITYPE, ALUOps.AND),
    AUIPC  -> List(N,   Y,    N,    N,    N, N, btDC,  opDC,   imm,    ImmFormat.UTYPE, ALUOps.ADD),
    LUI    -> List(N,   Y,    N,    N,    N, N, btDC,  opDC,   imm,    ImmFormat.UTYPE, ALUOps.DC),
    ORI    -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.ITYPE, ALUOps.OR),
    SLLI   -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.SHAMT, ALUOps.SLL),
    SLTI   -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.ITYPE, ALUOps.SLT),
    SLTIU  -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.ITYPE, ALUOps.SLTU),
    SRAI   -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.SHAMT, ALUOps.SRA),
    SRLI   -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.SHAMT, ALUOps.SRL),
    XORI   -> List(N,   Y,    N,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.ITYPE, ALUOps.XOR),

    // Memory
    LW     -> List(Y,   Y,    Y,    N,    N, N, btDC,  rs1,    imm,    ImmFormat.ITYPE, ALUOps.ADD),
    SW     -> List(N,   N,    N,    Y,    N, N, btDC,  rs1,    imm,    ImmFormat.STYPE, ALUOps.ADD),

    // Jump and link
    JAL    -> List(N,   Y,    N,    N,    N, Y, jump,  PC,     imm,    ImmFormat.JTYPE, ALUOps.ADD),
    JALR   -> List(N,   Y,    N,    N,    N, Y, jump,  rs1,    imm,    ImmFormat.ITYPE, ALUOps.ADD),

    // Branch
    BEQ    -> List(N,   N,    N,    N,    Y, N, beq,   rs1,    rs2,    ImmFormat.BTYPE, ALUOps.DC),
    BGE    -> List(N,   N,    N,    N,    Y, N, gte,   rs1,    rs2,    ImmFormat.BTYPE, ALUOps.DC),
    BGEU   -> List(N,   N,    N,    N,    Y, N, gteu,  rs1,    rs2,    ImmFormat.BTYPE, ALUOps.DC),
    BLT    -> List(N,   N,    N,    N,    Y, N, lt,    rs1,    rs2,    ImmFormat.BTYPE, ALUOps.DC),
    BLTU   -> List(N,   N,    N,    N,    Y, N, ltu,   rs1,    rs2,    ImmFormat.BTYPE, ALUOps.DC),
    BNE    -> List(N,   N,    N,    N,    Y, N, neq,   rs1,    rs2,    ImmFormat.BTYPE, ALUOps.DC),
  )


  val NOP = List(N, N, N, N, N, N, branchType.DC, rs1, rs2, ImmFormat.DC, ALUOps.DC)

  val decodedControlSignals = ListLookup(
    io.instruction.asUInt(),
    NOP,
    opcodeMap)

  io.controlSignals.memToReg   := decodedControlSignals(0)
  io.controlSignals.regWrite   := decodedControlSignals(1)
  io.controlSignals.memRead    := decodedControlSignals(2)
  io.controlSignals.memWrite   := decodedControlSignals(3)
  io.controlSignals.branch     := decodedControlSignals(4)
  io.controlSignals.jump       := decodedControlSignals(5)

  io.branchType := decodedControlSignals(6)
  io.op1Select  := decodedControlSignals(7)
  io.op2Select  := decodedControlSignals(8)
  io.immType    := decodedControlSignals(9)
  io.ALUop      := decodedControlSignals(10)
}
