package Ov1
import spire.math.{UInt => Uint}
import utilz._


object RISCVOPS {
  import RISCVutils._

  case class StateUpdate(r: Option[(Reg, Word)], m: Option[(Addr, Word)], pc: Addr)

  implicit class RegMapExt(val self: Map[Reg, Word]) extends AnyVal {
    // gets a map value for specified key casting as required type
    def updatedR(k: Reg, w: Word): Map[Reg,Word] = {
      if(k == 0)
        self
      else
        self.updated(k, w)
    }
  }


  object StateUpdate {
    def logReg(r: Reg, m: MachineState): StateUpdate = {
      if(r != 0)
        StateUpdate(Some((r, m.regs(r))), None, m.pc)
      else
        StateUpdate(None, None, m.pc)
    }
    def logMem(a: Addr, m: MachineState): StateUpdate =
      StateUpdate(None, Some((a, m.mem(a))), m.pc)
    def apply(m: MachineState): StateUpdate =
      StateUpdate(None, None, m.pc)
  }

  type MachineUpdate = MachineState => Either[RuntimeError, (MachineState, StateUpdate)]


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
    case BEQ(rs1, rs2, imm)  => s"BEQ ${rn(rs1)}, ${rn(rs2)}, $imm"
    case BNE(rs1, rs2, imm)  => s"BNE ${rn(rs1)}, ${rn(rs2)}, $imm"
    case BGE(rs1, rs2, imm)  => s"BGE ${rn(rs1)}, ${rn(rs2)}, $imm"
    case BLTU(rs1, rs2, imm) => s"BLTU ${rn(rs1)}, ${rn(rs2)}, $imm"
    case BGEU(rs1, rs2, imm) => s"BGEU ${rn(rs1)}, ${rn(rs2)}, $imm"
    case BLT(rs1, rs2, imm)  => s"BLT ${rn(rs1)}, ${rn(rs2)}, $imm"

    case JALR(rd, rs1, imm)  => s"JALR ${rn(rd)}, ${rn(rs1)}, $imm"
    case JAL(rd, imm)        => s"JAL ${rn(rd)}, $imm"

    case ADD(rd: Reg, rs1: Reg, rs2: Reg) =>  s"ADD ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)}"
    case SUB(rd: Reg, rs1: Reg, rs2: Reg) =>  s"SUB ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)}"
    case AND(rd: Reg, rs1: Reg, rs2: Reg) =>  s"AND ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)}"
    case XOR(rd: Reg, rs1: Reg, rs2: Reg) =>  s"XOR ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)}"
    case  OR(rd: Reg, rs1: Reg, rs2: Reg) =>  s"OR ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)} "

    case  SLLI(rd, rs1, imm) => s"SLLI ${rn(rd)}, ${rn(rs1)}, $imm "
    case  SRLI(rd, rs1, imm) => s"SRLI ${rn(rd)}, ${rn(rs1)}, $imm "
    case  SRAI(rd, rs1, imm) => s"SRAI ${rn(rd)}, ${rn(rs1)}, $imm "

    case  ADDI(rd, rs1, imm) => s"ADDI ${rn(rd)}, ${rn(rs1)}, $imm "
    case  ANDI(rd, rs1, imm) => s"ANDI ${rn(rd)}, ${rn(rs1)}, $imm "
    case  XORI(rd, rs1, imm) => s"XORI ${rn(rd)}, ${rn(rs1)}, $imm "
    case   ORI(rd, rs1, imm) => s" ORI ${rn(rd)}, ${rn(rs1)}, $imm "

    case  SLL(rd, rs1, rs2)   => s"SLL ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)}"
    case  SRL(rd, rs1, rs2)   => s"SRL ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)}"
    case  SRA(rd, rs1, rs2)   => s"SRA ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)}"
    case  SLTI(rd, rs1, imm)  => s"SLTI ${rn(rd)}, ${rn(rs1)}, $imm"
    case  SLTIU(rd, rs1, imm) => s"SLTIU ${rn(rd)}, ${rn(rs1)}, $imm"
    case  SLT(rd, rs1, rs2)   => s"SLT ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)} "
    case  SLTU(rd, rs1, rs2)  => s"SLTU ${rn(rd)}, ${rn(rs1)}, ${rn(rs2)}"
    case  LUI(rd, imm)        => s"LUI ${rn(rd)}, $imm"
    case  AUIPC(rd, imm)      => s"AUIPC ${rn(rd)}, $imm"

    case  SW(rs2, rs1, offset) => s"SW ${rn(rs2)}, $offset(${rn(rs2)})"
    case  LW(rd, rs1, offset)  => s"LW ${rn(rd)},  $offset(${rn(rs1)})"

    case  NOP => "NOP"
    case _ => "We done"
  }


  def describeBranch(rs1: Reg,
                     rs2: Reg,
                     imm: Imm,
                     cond: (Uint, Uint) => Boolean,
                     condString: String,
                     signed: Boolean = false): (MachineState, MachineState) => String = { case(old, next) =>

      if(cond(old.regs(rs1), old.regs(rs2))){
        s"since ${old.regs(rs1).show(signed)} $condString ${old.regs(rs2).show(signed)} PC is set to ${hs(old.pc)} + ${imm} = ${hs(next.pc)}"
      }
      else{
        s"since ${old.regs(rs1).show(signed)} $condString ${old.regs(rs2).show(signed)} is not met PC is set to ${hs(next.pc)}"
      }
  }

  def describeArithmetic(rd: Reg,
                         rs1: Reg,
                         rs2: Reg,
                         opString: String,
                         signed: Boolean = false): (MachineState, MachineState) => String = { case(old, next) =>

      s"r${rd} changed from ${old.regs(rd).show(signed)} to ${old.regs(rs1).show(signed)} $opString ${hs(old.regs(rs2))} = ${hs(next.regs(rd))}" ++
        s"\nPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
  }

  def describeArithmeticImm(rd: Reg,
                            rs1: Reg,
                            imm: Imm,
                            opString: String,
                            signed: Boolean = false): (MachineState, MachineState) => String = { case(old, next) =>

      s"r${rd} changed from ${old.regs(rd).show(signed)} to ${old.regs(rs1).show(signed)} $opString $imm = ${hs(next.regs(rd))}" ++
        s"\nPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
  }

  def describeShift(rd: Reg,
                    rs1: Reg,
                    rs2: Reg,
                    opString: String): (MachineState, MachineState) => String = { case(old, next) =>

      s"r${rd} changed from ${hs(old.regs(rd))} to r${hs(old.regs(rs1))} $opString ${hs(old.regs(rs2))}[0:4] = ${hs(next.regs(rd))}" ++
        s"\nPC changed froold ${hs(old.pc)} to ${hs(next.pc)}"
  }

  def describeShiftImm(rd: Reg,
                       rs1: Reg,
                       imm: Imm,
                       opString: String): (MachineState, MachineState) => String = { case(old, next) =>

      s"r${rd} changed from ${hs(old.regs(rd))} to r${hs(old.regs(rs1))} $opString r${imm}[0:4] = ${hs(next.regs(rd))}" ++
        s"\nPC changed from ${hs(old.pc)} to ${hs(next.pc)}"
  }

  def describeOp(op: OP): (MachineState, MachineState) => String = op match {
    case BEQ(rs1, rs2, imm) =>  describeBranch(rs1, rs2, imm, (x,y) => x.toInt == y.toInt, "==", true)
    case BNE(rs1, rs2, imm) =>  describeBranch(rs1, rs2, imm, (x,y) => x.toInt != y.toInt, "!=", true)
    case BGE(rs1, rs2, imm) =>  describeBranch(rs1, rs2, imm, (x,y) => x.toInt >= y.toInt, ">=", true)
    case BLT(rs1, rs2, imm) =>  describeBranch(rs1, rs2, imm, (x,y) => x.toInt <= y.toInt,  "<", true)
    case BLTU(rs1, rs2, imm) => describeBranch(rs1, rs2, imm, _<_, "==")
    case BGEU(rs1, rs2, imm) => describeBranch(rs1, rs2, imm, _>=_, ">=")

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
    case  SLTIU(rd, rs1, imm) => describeArithmeticImm(rd, rs1, imm, "<u", true)
    case  SLT(rd, rs1, rs2)   => describeArithmetic(rd, rs1, rs2, "<")
    case  SLTU(rd, rs1, rs2)  => describeArithmetic(rd, rs1, rs2, "<u", true)

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
                    cond: (Uint, Uint) => Boolean): MachineUpdate = m => {

    if(cond(m.regs(rs1), m.regs(rs2))) {
      val next = MachineState(m.mem, m.regs, Uint(m.pc.toInt + imm))
      Right((next, StateUpdate(next)))
    }
    else {
      val next = MachineState(m.mem, m.regs, m.pc + Uint(4))
      Right((next, StateUpdate(next)))
    }
  }


  def applyArithmeticOp(rd: Reg,
                        rs1: Reg,
                        rs2: Reg,
                        op: (Uint, Uint) => Uint): MachineUpdate = m => {
    val next = m.updateRegs(rd, op(m.regs(rs1), m.regs(rs2)))
    Right((next,StateUpdate.logReg(rd, next)))
  }


  def applyArithmeticOpImm(rd: Reg,
                           rs1: Reg,
                           imm: Imm,
                           op: (Uint, Uint) => Uint): MachineUpdate = m => {
    val next = m.updateRegs(rd, op(m.regs(rs1), Uint(imm)))
    Right((next,StateUpdate.logReg(rd, next)))
  }


  def applyShiftOp(rd: Reg,
                   rs1: Reg,
                   rs2: Reg,
                   op: (Uint, Uint) => Uint): MachineUpdate = m => {
    val next = m.updateRegs(rd, op(m.regs(rs1), m.regs(rs2)))
    Right((next,StateUpdate.logReg(rd, next)))
  }


  def applyShiftOpImm(rd: Reg,
                      rs1: Reg,
                      imm: Imm,
                      op: (Uint, Uint) => Uint): MachineUpdate = m => {
    val next = m.updateRegs(rd, op(m.regs(rs1), Uint(imm)))
    Right((next, StateUpdate.logReg(rd, next)))

  }

  def applyOperation(op: OP): MachineUpdate = op match {
    case BEQ(rs1, rs2, imm)  => applyBranchOp(rs1, rs2, imm, (x, y) => x == y)
    case BNE(rs1, rs2, imm)  => applyBranchOp(rs1, rs2, imm, (x, y) => x != y)
    case BGE(rs1, rs2, imm)  => applyBranchOp(rs1, rs2, imm, (x, y) => x.toInt >= y.toInt)
    case BGEU(rs1, rs2, imm) => applyBranchOp(rs1, rs2, imm, (x, y) => x >= y)
    case BLT(rs1, rs2, imm)  => applyBranchOp(rs1, rs2, imm, (x, y) => x.toInt <  y.toInt)
    case BLTU(rs1, rs2, imm) => applyBranchOp(rs1, rs2, imm, (x, y) => x < y)

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

    case  SLTI(rd, rs1, imm)  => applyArithmeticOpImm(rd, rs1, imm, (x, y) => if(x.toInt < y.toInt) Uint(1) else Uint(0))
    case  SLTIU(rd, rs1, imm) => applyArithmeticOpImm(rd, rs1, imm, (x, y) => if(x < y) Uint(1) else Uint(0))

    case JALR(rd, rs1, imm)  => m => {
      val next = MachineState(m.mem, m.regs.updatedR(rd, m.pc + Uint(4)), Uint((m.regs(rs1).toInt + imm) & 0xFFFFFFFE))
      Right((next, StateUpdate.logReg(rd, next)))
    }

    case JAL(rd, imm) => m => {
      val next = MachineState(m.mem, m.regs.updatedR(rd, m.pc + Uint(4)), Uint((m.pc.toInt + imm)))
      Right((next, StateUpdate.logReg(rd, next)))
    }

    case  LUI(rd, imm) => m => {
      val next = MachineState( m.mem, m.regs.updatedR(rd, Uint(imm << 12)), m.pc + Uint(4))
      Right((next, StateUpdate.logReg(rd, next)))
    }

    case  AUIPC(rd, imm) => m => {
      val next = MachineState( m.mem, m.regs.updatedR(rd, m.pc + Uint(imm << 12)), m.pc + Uint(4))
      Right((next, StateUpdate.logReg(rd, next)))
    }

    case  SW(rs2, rs1, offset) => m => {
      val address = Uint(offset + m.regs(rs1).toInt)
      if(address > Uint(4096))
        Left(s"Attempted illegal write at $address (from reg ${rn(rs1)} (with value ${m.regs(rs1)}) + $offset)")
      else {
        val next = MachineState(m.mem.updated(address, m.regs(rs2)), m.regs, m.pc + Uint(4))
        Right((next,  StateUpdate.logMem(address, next)))
      }
    }

    case  LW(rd, rs1, offset) => m => {
      val address = Uint(offset + m.regs(rs1).toInt)
      val inRange = if(address > Uint(4096))
                      Left(s"Attempted illegal read at $address (from reg ${rn(rs1)} (with value ${m.regs(rs1)}) + $offset)")
                    else
                      Right(address)

      val unInitializedErrorMsg =
        s"Attempted read at $address (from reg ${rn(rs1)} (with value ${m.regs(rs1)}) + $offset). PC address: ${hs(m.pc)}\n" ++
          "This is a LEGAL address, and according to spec should return whatever happens to be at this location.\n" ++
          "However YOUR program has not defined what should be on this address, and therefore it is highly unlikely\n" ++
          "That this is intended behavior. If you disagree feel free to edit the source code"

      for {
        address <- inRange
        loadedValue <- m.mem.lift(address).toRight(unInitializedErrorMsg)
      } yield {
        val next = MachineState(m.mem, m.regs.updatedR(rd, loadedValue), m.pc + Uint(4))
        ((next, StateUpdate.logReg(rd, next)))
      }
    }

    case NOP => m => {
      val next = MachineState(m.mem, m.regs, m.pc + Uint(4))
      Right((next, StateUpdate(next)))
    }

    case DONE => m => {
      val next = m.copy(pc = Uint(0xF01D1EF7))
      Right((next, StateUpdate(next)))
    }
  }

  def rn(i: Int): String = i match {
    case 0  => "zero"
    case 1  => "ra"
    case 2  => "sp"
    case 3  => "gp"
    case 4  => "tp"
    case 5  => "t0"
    case 6  => "t1"
    case 7  => "t2"
    case 8  => "fp"
    case 9  => "s1"

    case 10 => "a0"
    case 11 => "a1"
    case 12 => "a2"
    case 13 => "a3"
    case 14 => "a4"
    case 15 => "a5"
    case 16 => "a6"
    case 17 => "a7"
    case 18 => "s2"
    case 19 => "s3"

    case 20 => "s4"
    case 21 => "s5"
    case 22 => "s6"
    case 23 => "s7"
    case 24 => "s8"
    case 25 => "s9"
    case 26 => "s10"
    case 27 => "s11"
    case 28 => "t3"
    case 29 => "t4"

    case 30 => "t5"
    case 31 => "t6"
    case  _ => "ERROR"
  }
}
