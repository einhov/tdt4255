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

  case class JALR(rd: Reg, rs1: Reg, imm: Either[String, Imm]) extends asmOP
  object JALR {
    def apply(rd: Reg, rs1: Reg, imm: String): JALR =
      JALR(rd, rs1, Left(imm))
    def apply(rd: Reg, rs1: Reg, imm: Imm): JALR =
      JALR(rd, rs1, Right(imm))
  }

  case class JAL(rd: Reg, imm: Either[String, Imm]) extends asmOP
  object JAL {
    def apply(rd: Reg, imm: String): JAL =
      JAL(rd, Left(imm))
    def apply(rd: Reg, imm: Imm): JAL =
      JAL(rd, Right(imm))
  }

  case class JR(rs1: Reg) extends asmOP
  case class LI(rd: Reg, imm: Imm) extends asmOP
  case class J(imm: String) extends asmOP

  case class ADD(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class SUB(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class AND(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class XOR(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class  OR(rd: Reg, rs1: Reg, rs2: Reg) extends asmOP
  case class MOV(rd: Reg, rs1: Reg) extends asmOP

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
    case BEQ(rs1, rs2, imm)    => RISCVOPS.BEQ(rs1, rs2, (labels(imm) - address).toInt)
    case BNE(rs1, rs2, imm)    => RISCVOPS.BNE(rs1, rs2, (labels(imm) - address).toInt)
    case BGE(rs1, rs2, imm)    => RISCVOPS.BGE(rs1, rs2, (labels(imm) - address).toInt)
    case BLT(rs1, rs2, imm)    => RISCVOPS.BLT(rs1, rs2, (labels(imm) - address).toInt)
    case BLTU(rs1, rs2, imm)   => RISCVOPS.BLTU(rs1, rs2,(labels(imm) - address).toInt)
    case BGEU(rs1, rs2, imm)   => RISCVOPS.BGEU(rs1, rs2,(labels(imm) - address).toInt)

    case JALR(rd, rs1, imm)    => imm match {
      case Right(i) => RISCVOPS.JALR(rd, rs1, i.toInt)
      case Left(s)  => RISCVOPS.JALR(rd, rs1, (labels(s) - address).toInt)
    }
    case JAL(rd, imm)         => imm match {
      case Right(i) => RISCVOPS.JAL(rd,       i.toInt)
      case Left(s) => RISCVOPS.JAL(rd,       (labels(s) - address).toInt)
    }

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

      // pseudo ops
    case JR(rs1)              => RISCVOPS.JALR(0, rs1, 0)
    case LI(rd, imm)          => RISCVOPS.ADDI(rd, 0, imm)
    case MOV(rd, rs1)         => RISCVOPS.ADD(rd, 0, rs1)
    case J(imm)               => toRealOP(JAL(0, imm), labels, address)
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

object regNames {
  val x0  = 0
  val x1  = 1
  val x2  = 2
  val x3  = 3
  val x4  = 4
  val x5  = 5
  val x6  = 6
  val x7  = 7
  val x8  = 8
  val x9  = 9

  val x10 = 10
  val x11 = 11
  val x12 = 12
  val x13 = 13
  val x14 = 14
  val x15 = 15
  val x16 = 16
  val x17 = 17
  val x18 = 18
  val x19 = 19

  val x20 = 20
  val x21 = 21
  val x22 = 22
  val x23 = 23
  val x24 = 24
  val x25 = 25
  val x26 = 26
  val x27 = 27
  val x28 = 28
  val x29 = 29

  val x30 = 30
  val x31 = 31


  val zero = 0
  val ra   = 1
  val sp   = 2
  val gp   = 3
  val tp   = 4
  val t0   = 5
  val t1   = 6
  val t2   = 7
  val s0   = 8
  val fp   = 8
  val s1   = 9

  val a0   = 10
  val a1   = 11
  val a2   = 12
  val a3   = 13
  val a4   = 14
  val a5   = 15
  val a6   = 16
  val a7   = 17
  val s2   = 18
  val s3   = 19

  val s4   = 20
  val s5   = 21
  val s6   = 22
  val s7   = 23
  val s8   = 24
  val s9   = 25
  val s10  = 26
  val s11  = 27
  val t3   = 28
  val t4   = 29

  val t5   = 30
  val t6   = 31
}
