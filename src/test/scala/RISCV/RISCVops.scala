package Ov1
import spire.math.{UInt => Uint}


object RISCVOPS {
  import RISCVutils._

  sealed trait OP
  case class BEQ(rs1: Reg, rs2: Reg, imm: Imm) extends OP
  case class BNE(rs1: Reg, rs2: Reg, imm: Imm) extends OP
  case class BLT(rs1: Reg, rs2: Reg, imm: Imm) extends OP
  case class BGE(rs1: Reg, rs2: Reg, imm: Imm) extends OP
  case class BLTU(rs1: Reg, rs2: Reg, imm: Imm) extends OP
  case class BGEU(rs1: Reg, rs2: Reg, imm: Imm) extends OP

  case class JALR(rd: Reg, rs1: Reg, imm: Imm) extends OP
  case class JAL(rd: Reg, imm: Imm) extends OP

  case class ADD(rd: Reg, rs1: Reg, rs2: Reg) extends OP
  case class SUB(rd: Reg, rs1: Reg, rs2: Reg) extends OP
  case class AND(rd: Reg, rs1: Reg, rs2: Reg) extends OP
  case class XOR(rd: Reg, rs1: Reg, rs2: Reg) extends OP
  case class  OR(rd: Reg, rs1: Reg, rs2: Reg) extends OP

  case class ADDI(rd: Reg, rs1: Reg, imm: Imm) extends OP
  case class ANDI(rd: Reg, rs1: Reg, imm: Imm) extends OP
  case class XORI(rd: Reg, rs1: Reg, imm: Imm) extends OP
  case class  ORI(rd: Reg, rs1: Reg, imm: Imm) extends OP

  case class SLLI(rd: Reg, rs1: Reg, imm: Imm) extends OP
  case class SRLI(rd: Reg, rs1: Reg, imm: Imm) extends OP
  case class SRAI(rd: Reg, rs1: Reg, imm: Imm) extends OP

  case class SLL(rd: Reg, rs1: Reg, rs2: Reg) extends OP
  case class SRL(rd: Reg, rs1: Reg, rs2: Reg) extends OP
  case class SRA(rd: Reg, rs1: Reg, rs2: Reg) extends OP

  case class SLTI(rd: Reg, rs1: Reg, imm: Imm) extends OP
  case class SLTIU(rd: Reg, rs1: Reg, imm: Imm) extends OP
  case class SLT(rd: Reg, rs1: Reg, rs2: Reg) extends OP
  case class SLTU(rd: Reg, rs1: Reg, rs2: Reg) extends OP

  case class LUI(rd: Reg, imm: Imm) extends OP
  case class AUIPC(rd: Reg, imm: Imm) extends OP
  case class SW(rs2: Reg, rs1: Reg, offset: Imm) extends OP
  case class LW(rd: Reg, rs1: Reg, offset: Imm) extends OP

  case object NOP extends OP
  case object DONE extends OP



  def renderInstruction(op: OP): String = op match {
    case BEQ(rs1, rs2, imm)  => s"BEQ    $rs1, $rs2, $imm \t;; PC <- (r$rs1 == r$rs2) ? PC <- PC + imm : PC <- PC + 4"
    case BNE(rs1, rs2, imm)  => s"BNE    $rs1, $rs2, $imm \t;; PC <- (r$rs1 != r$rs2) ? PC <- PC + imm : PC <- PC + 4"
    case BGE(rs1, rs2, imm)  => s"BGE    $rs1, $rs2, $imm \t;; PC <- (r$rs1 >= r$rs2) ? PC <- PC + imm : PC <- PC + 4"
    case BLTU(rs1, rs2, imm) => s"BLTU   $rs1, $rs2, $imm \t;; PC <- (r$rs1 <  r$rs2) ? PC <- PC + imm : PC <- PC + 4"
    case BGEU(rs1, rs2, imm) => s"BGEU   $rs1, $rs2, $imm \t;; PC <- (r$rs1 >= r$rs2) ? PC <- PC + imm : PC <- PC + 4"
    case BLT(rs1, rs2, imm)  => s"BLT    $rs1, $rs2, $imm \t;; PC <- (r$rs1 <  r$rs2) ? PC <- PC + imm : PC <- PC + 4"

    case JALR(rd, rs1, imm)  => s"JALR   $rd, $rs1, $imm  \t;; r$rd <- PC + Uint(4); PC <- r$rs1 + imm($imm)"
    case JAL(rd, imm)        => s"JAL    $rd, $imm        \t;; r$rd <- PC + Uint(4); PC <- PC + imm($imm)"

    case ADD(rd: Reg, rs1: Reg, rs2: Reg) =>  s"ADD    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 + r$rs2 "
    case SUB(rd: Reg, rs1: Reg, rs2: Reg) =>  s"SUB    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 - r$rs2 "
    case AND(rd: Reg, rs1: Reg, rs2: Reg) =>  s"AND    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 & r$rs2 "
    case XOR(rd: Reg, rs1: Reg, rs2: Reg) =>  s"XOR    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 ^ r$rs2 "
    case  OR(rd: Reg, rs1: Reg, rs2: Reg) =>  s"OR     $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 | r$rs2 "

    case  SLLI(rd, rs1, imm) => s"SLLI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 << imm($imm)[0:4]"
    case  SRLI(rd, rs1, imm) => s"SRLI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 >>> imm($imm)[0:4]"
    case  SRAI(rd, rs1, imm) => s"SRAI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 >> imm($imm)[0:4]"

    case  ADDI(rd, rs1, imm) => s"ADDI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 + imm($imm) "
    case  ANDI(rd, rs1, imm) => s"ANDI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 & imm($imm) "
    case  XORI(rd, rs1, imm) => s"XORI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 ^ imm($imm) "
    case   ORI(rd, rs1, imm) => s" ORI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 | imm($imm) "

    case  SLL(rd, rs1, rs2)   => s"SLL    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 << r$rs2[0:4] "
    case  SRL(rd, rs1, rs2)   => s"SRL    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 >>> r$rs2[0:4] "
    case  SRA(rd, rs1, rs2)   => s"SRA    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 >> r$rs2[0:4] "
    case  SLTI(rd, rs1, imm)  => s"SLTI   $rd, $rs1, $imm  \t;; r$rd <- (r$rs1 > imm($imm)) ? 1 : 0"
    case  SLTIU(rd, rs1, imm) => s"SLTIU  $rd, $rs1, $imm  \t;; r$rd <- (r$rs1 > imm($imm)) ? 1 : 0"
    case  SLT(rd, rs1, rs2)   => s"SLT    $rd, $rs1, $rs2  \t;; r$rd <- (r$rs1 > r$rs2) ? 1 : 0"
    case  SLTU(rd, rs1, rs2)  => s"SLTU   $rd, $rs1, $rs2  \t;; r$rd <- (r$rs1 > r$rs2) ? 1 : 0"
    case  LUI(rd, imm)        => s"LUI    $rd, $imm        \t;; r$rd <- imm($imm) << 12 "
    case  AUIPC(rd, imm)      => s"AUIPC  $rd, $imm        \t;; r$rd <- PC + (imm($imm) << 12) "

    case  SW(rs2, rs1, offset) => s"SW     $rs2, $offset($rs2) ;; MEM[r$rs1 + $offset] <- r$rs2"
    case  LW(rd, rs1, offset)  => s"LW     $rd,  $offset($rs1) ;; r$rd <- MEM[r$rs1 + $offset]"

    case  NOP => "NOP"
    case _ => "We done"
  }


  def describeBranch(rs1: Reg,
                     rs2: Reg,
                     imm: Imm,
                     cond: (Uint, Uint) => Boolean,
                     condString: String): (MachineState, MachineState) => String = { case(old, next) =>

    if(cond(old.regs(rs1), old.regs(rs2))){
      s"since ${hs(old.regs(rs1))} $condString r${hs(old.regs(rs2))} PC is set to ${hs(old.pc)} + ${imm} = ${hs(next.pc)}"
    }
    else{
      s"since ${hs(old.regs(rs1))} $condString r${hs(old.regs(rs2))} is not met PC is set to ${hs(next.pc)}"
    }
  }

  def describeArithmetic(rd: Reg,
                         rs1: Reg,
                         rs2: Reg,
                         opString: String): (MachineState, MachineState) => String = { case(old, next) =>

      s"r${rd} changed from ${hs(old.regs(rd))} to ${hs(old.regs(rs1))} $opString ${hs(old.regs(rs2))} = ${hs(next.regs(rd))}" ++
        s"\tPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
  }

  def describeArithmeticImm(rd: Reg,
                            rs1: Reg,
                            imm: Imm,
                            opString: String): (MachineState, MachineState) => String = { case(old, next) =>

      s"r${rd} changed from ${hs(old.regs(rd))} to r${hs(old.regs(rs1))} $opString $imm = ${hs(next.regs(rd))}" ++
        s"\tPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
  }

  def describeShift(rd: Reg,
                    rs1: Reg,
                    rs2: Reg,
                    opString: String): (MachineState, MachineState) => String = { case(old, next) =>

      s"r${rd} changed from ${hs(old.regs(rd))} to r${hs(old.regs(rs1))} $opString r${hs(old.regs(rs2))}[0:4] = ${hs(next.regs(rd))}" ++
        s"\tPC changed froold ${hs(old.pc)} to ${hs(next.pc)}"
  }

  def describeShiftImm(rd: Reg,
                       rs1: Reg,
                       imm: Imm,
                       opString: String): (MachineState, MachineState) => String = { case(old, next) =>

      s"r${rd} changed from ${hs(old.regs(rd))} to r${hs(old.regs(rs1))} $opString r${imm}[0:4] = ${hs(next.regs(rd))}" ++
        s"\nPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
  }

  def describeOp(op: OP): (MachineState, MachineState) => String = op match {
    case BEQ(rs1, rs2, imm) =>  describeBranch(rs1, rs2, imm, (x,y) => x.toInt == y.toInt, "==")
    case BNE(rs1, rs2, imm) =>  describeBranch(rs1, rs2, imm, (x,y) => x.toInt != y.toInt, "!=")
    case BGE(rs1, rs2, imm) =>  describeBranch(rs1, rs2, imm, (x,y) => x.toInt >= y.toInt, ">=")
    case BLT(rs1, rs2, imm) =>  describeBranch(rs1, rs2, imm, (x,y) => x.toInt <= y.toInt,  "<")
    case BLTU(rs1, rs2, imm) => describeBranch(rs1, rs2, imm, _<_, "==")
    case BGEU(rs1, rs2, imm) => describeBranch(rs1, rs2, imm, _>=_, "==")

    case ADD(rd: Reg, rs1: Reg, rs2: Reg) => describeArithmetic(rd,rs1, rs2, "+")
    case SUB(rd: Reg, rs1: Reg, rs2: Reg) => describeArithmetic(rd,rs1, rs2, "-")
    case AND(rd: Reg, rs1: Reg, rs2: Reg) => describeArithmetic(rd,rs1, rs2, "&")
    case XOR(rd: Reg, rs1: Reg, rs2: Reg) => describeArithmetic(rd,rs1, rs2, "^")
    case  OR(rd: Reg, rs1: Reg, rs2: Reg) => describeArithmetic(rd,rs1, rs2, "|")

    case  SLLI(rd, rs1, imm) => describeShiftImm(rd, rs1, imm, "<<")
    case  SRLI(rd, rs1, imm) => describeShiftImm(rd, rs1, imm, ">>")
    case  SRAI(rd, rs1, imm) => describeShiftImm(rd, rs1, imm, ">>>")

    case  ADDI (rd, rs1, imm) => describeArithmeticImm (rd, rs1, imm, "+")
    case  ANDI (rd, rs1, imm) => describeArithmeticImm (rd, rs1, imm, "&")
    case  XORI (rd, rs1, imm) => describeArithmeticImm (rd, rs1, imm, "^")
    case   ORI (rd, rs1, imm) => describeArithmeticImm (rd, rs1, imm, "|")

    case  SLL(rd, rs1, rs2) => describeShift(rd, rs1, rs2, "<<")
    case  SRL(rd, rs1, rs2) => describeShift(rd, rs1, rs2, ">>")
    case  SRA(rd, rs1, rs2) => describeShift(rd, rs1, rs2, ">>>")

    case  SLTI(rd, rs1, imm)  => describeArithmeticImm(rd, rs1, imm, "<")
    case  SLTIU(rd, rs1, imm) => describeArithmeticImm(rd, rs1, imm, "<u")
    case  SLT(rd, rs1, rs2)   => describeArithmetic(rd, rs1, rs2, "<")
    case  SLTU(rd, rs1, rs2)  => describeArithmetic(rd, rs1, rs2, "<u")

    case  LUI(rd, imm) => { case(old, next) =>
      s"r${rd} changed from ${hs(old.regs(rd))} to r${imm} << 12 = ${hs(next.regs(rd))}" ++
        s"\tPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
    }

    case  AUIPC(rd, imm) => { case(old, next) =>
      s"r${rd} changed from ${hs(old.regs(rd))} to r${hs(old.pc)} << 12 = ${hs(next.regs(rd))}" ++
        s"\tPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
    }

    case JALR(rd, rs1, imm) => { case(old, next) =>
      s"r${rd} changed from ${hs(old.regs(rd))} to ${hs(next.regs(rd))}" ++
        s"\nPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
    }
    case JAL(rd, imm) => { case(old, next) =>
      s"r${rd} changed from ${hs(old.regs(rd))} to ${hs(next.regs(rd))}" ++
        s"\nPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
    }

    case  SW(rs2, rs1, offset) => { case(old, next) =>
      val address = Uint(offset + old.regs(rs1).toInt)
      s"M[${address}] changed from ${old.mem.lift(address).getOrElse(0)} to ${next.mem(address)}" ++
      s"\nPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
    }

    case  LW(rd, rs1, offset) => { case(old, next) =>
      val address = Uint(offset + old.regs(rs1).toInt)
      s"r${rd} changed from ${old.regs(rd)} to ${next.regs(rd)} by loading from M[$address]" ++
      s"\nPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
    }

    case  NOP => { case(old, next) => "NOP" }

    case DONE => { case(old, next) => "DONE" }
  }


  def applyBranchOp(rs1: Reg,
                    rs2: Reg,
                    imm: Imm,
                    cond: (Uint, Uint) => Boolean
  ): MachineState => Either[RuntimeError,MachineState] = m => {
    if(cond(m.regs(rs1), m.regs(rs2)))
      Right(MachineState(m.mem, m.regs, Uint(m.pc.toInt + imm)))
    else
      Right(MachineState(m.mem, m.regs, m.pc + Uint(4)))
  }


  def applyArithmeticOp(rd: Reg,
                        rs1: Reg,
                        rs2: Reg,
                        op: (Uint, Uint) => Uint): MachineState => Either[RuntimeError,MachineState] = m => {
    Right(m.updateRegs(rd, op(m.regs(rs1), m.regs(rs2))))
  }


  def applyArithmeticOpImm(rd: Reg,
                           rs1: Reg,
                           imm: Imm,
                           op: (Uint, Uint) => Uint): MachineState => Either[RuntimeError,MachineState] = m => {
    Right(m.updateRegs(rd, op(m.regs(rs1), Uint(imm))))
  }


  def applyShiftOp(rd: Reg,
                   rs1: Reg,
                   rs2: Reg,
                   op: (Uint, Uint) => Uint): MachineState => Either[RuntimeError,MachineState] = m => {
    Right(m.updateRegs(rd, op(m.regs(rs1), m.regs(rs2))))
  }


  def applyShiftOpImm(rd: Reg,
                      rs1: Reg,
                      imm: Imm,
                      op: (Uint, Uint) => Uint): MachineState => Either[RuntimeError,MachineState] = m => {
    Right(m.updateRegs(rd, op(m.regs(rs1), Uint(imm))))
  }


  def applyOperation(op: OP): MachineState => Either[RuntimeError,MachineState] = op match {
    case BEQ(rs1, rs2, imm)  => applyBranchOp(rs1, rs2, imm, (x, y) => x == y)
    case BNE(rs1, rs2, imm)  => applyBranchOp(rs1, rs2, imm, (x, y) => x != y)
    case BGE(rs1, rs2, imm)  => applyBranchOp(rs1, rs2, imm, (x, y) => x.toInt >= y.toInt)
    case BLT(rs1, rs2, imm)  => applyBranchOp(rs1, rs2, imm, (x, y) => x.toInt <  y.toInt)
    case BLTU(rs1, rs2, imm) => applyBranchOp(rs1, rs2, imm, (x, y) => x >= y)
    case BGEU(rs1, rs2, imm) => applyBranchOp(rs1, rs2, imm, (x, y) => x <  y)

    case ADD(rd: Reg, rs1: Reg, rs2: Reg) => applyArithmeticOp(rd, rs1, rs2, _+_)
    case SUB(rd: Reg, rs1: Reg, rs2: Reg) => applyArithmeticOp(rd, rs1, rs2, _-_)
    case AND(rd: Reg, rs1: Reg, rs2: Reg) => applyArithmeticOp(rd, rs1, rs2, _&_)
    case XOR(rd: Reg, rs1: Reg, rs2: Reg) => applyArithmeticOp(rd, rs1, rs2, _^_)
    case  OR(rd: Reg, rs1: Reg, rs2: Reg) => applyArithmeticOp(rd, rs1, rs2, _|_)

    case  SLLI(rd, rs1, imm) => applyArithmeticOpImm(rd, rs1, imm, (x, y) => Uint(x.toInt << (y.toInt & 31)))
    case  SRLI(rd, rs1, imm) => applyArithmeticOpImm(rd, rs1, imm, (x, y) => Uint(x.toInt >>> (y.toInt & 31)))
    case  SRAI(rd, rs1, imm) => applyArithmeticOpImm(rd, rs1, imm, (x, y) => Uint(x.toInt >> (y.toInt & 31)))

    case  ADDI(rd, rs1, imm) => applyArithmeticOpImm(rd, rs1, imm, _+_)
    case  ANDI(rd, rs1, imm) => applyArithmeticOpImm(rd, rs1, imm, _&_)
    case  XORI(rd, rs1, imm) => applyArithmeticOpImm(rd, rs1, imm, _^_)
    case   ORI(rd, rs1, imm) => applyArithmeticOpImm(rd, rs1, imm, _|_)

    case  SLL(rd, rs1, rs2)   => applyArithmeticOp(rd, rs1, rs2, (x, y) => Uint(x.toInt << (y.toInt & 31)))
    case  SRL(rd, rs1, rs2)   => applyArithmeticOp(rd, rs1, rs2, (x, y) => Uint(x.toInt >>> (y.toInt & 31)))
    case  SRA(rd, rs1, rs2)   => applyArithmeticOp(rd, rs1, rs2, (x, y) => Uint(x.toInt >> (y.toInt & 31)))

    case  SLT(rd, rs1, rs2)   => applyArithmeticOp(rd, rs1, rs2, (x, y) => if(x.toInt < y.toInt) Uint(1) else Uint(0))
    case  SLTU(rd, rs1, rs2)  => applyArithmeticOp(rd, rs1, rs2, (x, y) => if(x < y) Uint(1) else Uint(0))

    case  SLTI(rd, rs1, imm)  => applyArithmeticOpImm(rd, rs1, imm, (x, y) => if(x < y) Uint(1) else Uint(0))
    case  SLTIU(rd, rs1, imm) => applyArithmeticOpImm(rd, rs1, imm, (x, y) => if(x < y) Uint(1) else Uint(0))

    case JALR(rd, rs1, imm)  => m => Right(MachineState(m.mem, m.regs.updated(rd, m.pc + Uint(4)), Uint(m.pc.toInt + m.regs(rs1).toInt + imm)))
    case JAL(rd, imm)        => m => Right(MachineState(m.mem, m.regs.updated(rd, m.pc + Uint(4)), Uint(m.pc.toInt + imm)))

    case  LUI(rd, imm)        => m => Right(MachineState( m.mem, m.regs.updated(rd, Uint(imm << 12)), m.pc + Uint(4)))
    case  AUIPC(rd, imm)      => m => Right(MachineState( m.mem, m.regs.updated(rd, m.pc << 12), m.pc + Uint(4)))

    case  SW(rs2, rs1, offset) => m => {
      val address = Uint(offset + m.regs(rs1).toInt)
      if(address > Uint(4096))
        Left(s"Attempted illegal write at $address (from reg $rs1 (with value ${m.regs(rs1)}) + $offset)")
      else
        Right(MachineState(m.mem.updated(address, m.regs(rs2)), m.regs, m.pc + Uint(4)))
      
    }
    case  LW(rd, rs1, offset) => m => {
      val address = Uint(offset + m.regs(rs1).toInt)
      val inRange = if(address > Uint(4096))
                      Left(s"Attempted illegal read at $address (from reg $rs1 (with value ${m.regs(rs1)}) + $offset)")
                    else
                      Right(address)

      val unInitializedErrorMsg =
        s"Attempted read at $address (from reg $rs1 (with value ${m.regs(rs1)}) + $offset)\n" ++
          "This is a LEGAL address, and according to spec should return whatever happens to be at this location.\n" ++
          "However YOUR program has not defined what should be on this address, and therefore it is highly unlikely\n" ++
          "That this is intended behavior. If you disagree feel free to edit the source code"

      for {
        address <- inRange
        loadedValue <- m.mem.lift(address).toRight(unInitializedErrorMsg)
      } yield MachineState(m.mem, m.regs.updated(rd, loadedValue), m.pc + Uint(4))
    }

    case NOP => m => Right(MachineState(m.mem, m.regs, m.pc + Uint(4)))
    case DONE => m => Right(m.copy(pc = Uint(0xF01D1EF7)))
  }
}
