package Ov1
import atto._, Atto._
import cats.implicits._

import spire.math.{UInt => Uint}
import spire.syntax.literals._

object RISCVasm {
  import RISCVutils._

  sealed trait asmOP

  case class BEQ(rs1: Reg,  rs2: Reg, imm: String) extends asmOP
  case class BNE(rs1: Reg,  rs2: Reg, imm: String) extends asmOP
  case class BLT(rs1: Reg,  rs2: Reg, imm: String) extends asmOP
  case class BGE(rs1: Reg,  rs2: Reg, imm: String) extends asmOP
  case class BLTU(rs1: Reg, rs2: Reg, imm: String) extends asmOP
  case class BGEU(rs1: Reg, rs2: Reg, imm: String) extends asmOP

  case class JALR(rd: Reg, rs1: Reg, imm: String) extends asmOP
  case class JAL(rd: Reg, imm: String) extends asmOP

  case class ADD(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class SUB(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class AND(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class XOR(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class  OR(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP

  case class ADDI(rd: Reg, rs1: Reg, imm: Imm) extends asmOP
  case class ANDI(rd: Reg, rs1: Reg, imm: Imm) extends asmOP
  case class XORI(rd: Reg, rs1: Reg, imm: Imm) extends asmOP
  case class  ORI(rd: Reg, rs1: Reg, imm: Imm) extends asmOP

  case class SLLI(rd: Reg, rs1: Reg, imm: Imm) extends asmOP
  case class SRLI(rd: Reg, rs1: Reg, imm: Imm) extends asmOP
  case class SRAI(rd: Reg, rs1: Reg, imm: Imm) extends asmOP

  case class SLL(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class SRL(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class SRA(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP

  case class SLTI(rd: Reg, rs1: Reg, imm: Imm) extends asmOP
  case class SLTIU(rd: Reg, rs1: Reg, imm: Imm) extends asmOP
  case class SLT(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class SLTU(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP

  case class LUI(rd: Reg, imm: Imm) extends asmOP
  case class AUIPC(rd: Reg, imm: Imm) extends asmOP
  case class SW(rs2: Reg, rs1: Reg, offset: Imm) extends asmOP
  case class LW(rd: Reg, rs1: Reg, offset: Imm) extends asmOP

  case class LABEL(name: String) extends asmOP

  case object NOP extends asmOP
  case object DONE extends asmOP

  def toRealOP(asmOP: asmOP, labels: Map[String, Addr], address: Addr): Ov1.RISCVOPS.OP = asmOP match {
    case BEQ(rs1, rs2, imm)  => RISCVOPS.BEQ(rs1, rs2, (labels(imm) - address).toInt)
    case BNE(rs1, rs2, imm)  => RISCVOPS.BNE(rs1, rs2, (labels(imm) - address).toInt)
    case BGE(rs1, rs2, imm)  => RISCVOPS.BGE(rs1, rs2, (labels(imm) - address).toInt)
    case BLT(rs1, rs2, imm)  => RISCVOPS.BLT(rs1, rs2, (labels(imm) - address).toInt)
    case BLTU(rs1, rs2, imm) => RISCVOPS.BLTU(rs1, rs2,(labels(imm) - address).toInt)
    case BGEU(rs1, rs2, imm) => RISCVOPS.BGEU(rs1, rs2,(labels(imm) - address).toInt)

    case JALR(rd, rs1, imm)  => RISCVOPS.JALR(rd, rs1, (labels(imm) - address).toInt)
    case JAL(rd, imm)        => RISCVOPS.JAL(rd,       (labels(imm) - address).toInt)

    case ADD(rd, rs1, rs2)    => RISCVOPS.ADD(rd, rs1, rs2)
    case SUB(rd, rs1, rs2)    => RISCVOPS.SUB(rd, rs1, rs2)
    case AND(rd, rs1, rs2)    => RISCVOPS.AND(rd, rs1, rs2)
    case XOR(rd, rs1, rs2)    => RISCVOPS.XOR(rd, rs1, rs2)
    case OR(rd, rs1, rs2)     => RISCVOPS.OR(rd, rs1, rs2)
    case SLLI(rd, rs1, imm)   => RISCVOPS.SLLI(rd, rs1, imm)
    case SRLI(rd, rs1, imm)   => RISCVOPS.SRLI(rd, rs1, imm)
    case SRAI(rd, rs1, imm)   => RISCVOPS.SRAI(rd, rs1, imm)
    case ADDI(rd, rs1, imm)   => RISCVOPS.ADDI(rd, rs1, imm)
    case ANDI(rd, rs1, imm)   => RISCVOPS.ANDI(rd, rs1, imm)
    case XORI(rd, rs1, imm)   => RISCVOPS.XORI(rd, rs1, imm)
    case ORI(rd, rs1, imm)    => RISCVOPS.ORI(rd, rs1, imm)
    case SLL(rd, rs1, rs2)    => RISCVOPS.SLL(rd, rs1, rs2)
    case SRL(rd, rs1, rs2)    => RISCVOPS.SRL(rd, rs1, rs2)
    case SRA(rd, rs1, rs2)    => RISCVOPS.SRA(rd, rs1, rs2)
    case SLT(rd, rs1, rs2)    => RISCVOPS.SLT(rd, rs1, rs2)
    case SLTU(rd, rs1, rs2)   => RISCVOPS.SLTU(rd, rs1, rs2)
    case SLTI(rd, rs1, imm)   => RISCVOPS.SLTI(rd, rs1, imm)
    case SLTIU(rd, rs1, imm)  => RISCVOPS.SLTIU(rd, rs1, imm)
    case LUI(rd, imm)         => RISCVOPS.LUI(rd, imm)
    case AUIPC(rd, imm)       => RISCVOPS.AUIPC(rd, imm)
    case SW(rs2, rs1, offset) => RISCVOPS.SW(rs2, rs1, offset)
    case LW(rd, rs1, offset)  => RISCVOPS.LW(rd, rs1, offset)
    case NOP                  => RISCVOPS.NOP
    case DONE                 => RISCVOPS.DONE
    case _ => RISCVOPS.NOP
  }


  def getLabels(ops: List[asmOP]): Map[String, Addr] = {
    ops.foldLeft((Map[String, Addr](), Uint(0))){ case((acc, addr), op) =>
      op match {
        case LABEL(address) => (acc.updated(address, addr), addr)
        case _ => (acc, addr + Uint(4))
      }
    }._1
  }


  def toRealOpsWithNOP(ops: List[asmOP]): List[RISCVOPS.OP] = {
    val labels = getLabels(ops).mapValues(_*Uint(5))
    val filtered = ops.filterNot( _ match {
                                case LABEL(_) => true
                                case _ => false
                              })

    filtered.zipWithIndex.flatMap{ case(op, idx) =>
      toRealOP(op, labels, Uint(idx*4*5)) :: List.fill(4)(RISCVOPS.NOP)
    }
  }
}
