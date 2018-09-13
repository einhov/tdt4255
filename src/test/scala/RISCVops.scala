package Ov1
import atto._, Atto._
import cats.implicits._

import spire.math.{UInt => Uint}
import spire.syntax.literals._


object RISCVOPS {

  type Reg = Int
  type Imm = Uint

  case class MachineState(mem: Map[Uint, Uint], regs: Map[Int, Uint], pc: Uint)

  def hs(u: Uint): String = {
    s"0x${u.toInt.toHexString.toUpperCase()}"
  }

  sealed trait OP {
    def show: String
    def run(m: MachineState): Either[String, (MachineState, String)]
  }


  case class BEQ(rs1: Reg, rs2: Reg, imm: Imm) extends OP   {
    def show = s"BEQ    $rs1, $rs2, $imm \t;; PC <- (r$rs1 == r$rs2) ? PC <- PC + imm : PC <- PC + 4"
    def run(m: MachineState): Either[String,(MachineState,String)] = {
      if(m.regs(rs1) == m.regs(rs2))  {
        val next = MachineState(m.mem, m.regs, m.pc + imm)
        val s = s"since ${hs(m.regs(rs1))} == r${hs(m.regs(rs2))} PC is set to ${hs(m.pc)} + ${imm} = ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
      else {
        val next = MachineState(m.mem, m.regs, m.pc + Uint(4))
        val s = s"since ${hs(m.regs(rs1))} == r${hs(m.regs(rs2))} is not met PC is set to ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
    }
  }
  case class BNE(rs1: Reg, rs2: Reg, imm: Imm) extends OP   {
    def show = s"BNE    $rs1, $rs2, $imm \t;; PC <- (r$rs1 != r$rs2) ? PC <- PC + imm : PC <- PC + 4"
    def run(m: MachineState): Either[String,(MachineState,String)] = {
      if(m.regs(rs1) != m.regs(rs2))  {
        val next = MachineState(m.mem, m.regs, m.pc + imm)
        val s = s"since ${hs(m.regs(rs1))} != r${hs(m.regs(rs2))} PC is set to ${hs(m.pc)} + ${imm} = ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
      else {
        val next = MachineState(m.mem, m.regs, m.pc + Uint(4))
        val s = s"since ${hs(m.regs(rs1))} != r${hs(m.regs(rs2))} is not met PC is set to ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
    }
  }
  case class BLT(rs1: Reg, rs2: Reg, imm: Imm) extends OP   {
    def show = s"BLT    $rs1, $rs2, $imm \t;; PC <- (r$rs1 < r$rs2)  ? PC <- PC + imm : PC <- PC + 4"
    def run(m: MachineState): Either[String,(MachineState,String)] = {
      if(m.regs(rs1).toInt < m.regs(rs2).toInt)  {
        val next = MachineState(m.mem, m.regs, m.pc + imm)
        val s = s"since ${hs(m.regs(rs1))} < r${hs(m.regs(rs2))} PC is set to ${hs(m.pc)} + ${imm} = ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
      else {
        val next = MachineState(m.mem, m.regs, m.pc + Uint(4))
        val s = s"since ${hs(m.regs(rs1))} < r${hs(m.regs(rs2))} is not met PC is set to ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
    }
  }
  case class BGE(rs1: Reg, rs2: Reg, imm: Imm) extends OP   {
    def show = s"BGE    $rs1, $rs2, $imm \t;; PC <- (r$rs1 >= r$rs2) ? PC <- PC + imm : PC <- PC + 4"
    def run(m: MachineState): Either[String,(MachineState,String)] = {
      if(m.regs(rs1).toInt >= m.regs(rs2).toInt)  {
        val next =      MachineState(m.mem, m.regs, m.pc + imm)
        val s = s"since ${hs(m.regs(rs1))} >= r${hs(m.regs(rs2))} PC is set to ${hs(m.pc)} + ${imm} = ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
      else {
        val next = MachineState(m.mem, m.regs, m.pc + Uint(4))
        val s = s"since ${hs(m.regs(rs1))} >= r${hs(m.regs(rs2))} is not met PC is set to ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
    }
  }
  case class BLTU(rs1: Reg, rs2: Reg, imm: Imm) extends OP  {
    def show = s"BLTU   $rs1, $rs2, $imm \t;; PC <- (r$rs1 < r$rs2)  ? PC <- PC + imm : PC <- PC + 4"
    def run(m: MachineState): Either[String,(MachineState,String)] = {
      if(m.regs(rs1) < m.regs(rs2))  {
        val next = MachineState(m.mem, m.regs, m.pc + imm)
        val s = s"since ${hs(m.regs(rs1))} < r${hs(m.regs(rs2))} PC is set to ${hs(m.pc)} + ${imm} = ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
      else {
        val next = MachineState(m.mem, m.regs, m.pc + Uint(4))
        val s = s"since ${hs(m.regs(rs1))} < r${hs(m.regs(rs2))} is not met PC is set to ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
    }
  }
  case class BGEU(rs1: Reg, rs2: Reg, imm: Imm) extends OP  {
    def show = s"BGEU   $rs1, $rs2, $imm \t;; PC <- (r$rs1 >= r$rs2) ? PC <- PC + imm : PC <- PC + 4"
    def run(m: MachineState): Either[String,(MachineState,String)] = {
      if(m.regs(rs1) >= m.regs(rs2))  {
        val next = MachineState(m.mem, m.regs, m.pc + imm)
        val s = s"since ${hs(m.regs(rs1))} >= r${hs(m.regs(rs2))} PC is set to ${hs(m.pc)} + ${imm} = ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
      else {
        val next = MachineState(m.mem, m.regs, m.pc + Uint(4))
        val s = s"since ${hs(m.regs(rs1))} >= r${hs(m.regs(rs2))} is not met PC is set to ${hs(next.pc)}"
        Right((next, show ++ "\n" ++ s))
      }
    }
  }


  case class JALR(rd: Reg, rs1: Reg, imm: Imm) extends OP   {
    def show = s"JALR   $rd, $rs1, $imm  \t;; r$rd <- PC + Uint(4); PC <- r$rs1 + imm($imm)"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.pc + Uint(4)), m.pc + m.regs(rs1) + imm)
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to ${hs(next.regs(rd))}" ++
        s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class JAL(rd: Reg, imm: Imm) extends OP              {
    def show = s"JAL    $rd, $imm        \t;; r$rd <- PC + Uint(4); PC <- PC + imm($imm)"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.pc + Uint(4)), m.pc + imm)
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to ${hs(next.regs(rd))}" ++
        s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }


  case class ADD(rd: Reg, rs1: Reg, rs2: Reg) extends OP    {
    def show = s"ADD    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 + r$rs2 "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.regs(rs1) + m.regs(rs2)), m.pc + Uint(4))
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to ${hs(m.regs(rs1))} + ${hs(m.regs(rs2))} = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class SUB(rd: Reg, rs1: Reg, rs2: Reg) extends OP    {
    def show = s"SUB    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 - r$rs2 "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.regs(rs1) - m.regs(rs2)), m.pc + Uint(4))
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to ${hs(m.regs(rs1))} - ${hs(m.regs(rs2))} = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class AND(rd: Reg, rs1: Reg, rs2: Reg) extends OP    {
    def show = s"AND    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 & r$rs2 "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.regs(rs1) & m.regs(rs2)), m.pc + Uint(4))
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} & r${hs(m.regs(rs2))} = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class XOR(rd: Reg, rs1: Reg, rs2: Reg) extends OP    {
    def show = s"XOR    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 ^ r$rs2 "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.regs(rs1) ^ m.regs(rs2)), m.pc + Uint(4))
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} ^ r${hs(m.regs(rs2))} = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class  OR(rd: Reg, rs1: Reg, rs2: Reg) extends OP    {
    def show = s"OR     $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 | r$rs2 "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.regs(rs1) | m.regs(rs2)), m.pc + Uint(4))
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} | r${hs(m.regs(rs2))} = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }


  case class ADDI(rd: Reg, rs1: Reg, imm: Imm) extends OP   {
    def show = s"ADDI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 + imm($imm) "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.regs(rs1) + imm), m.pc + Uint(4))
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} + $imm = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class ANDI(rd: Reg, rs1: Reg, imm: Imm) extends OP   {
    def show = s"ANDI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 & imm($imm) "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.regs(rs1) & imm), m.pc + Uint(4))
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} & $imm = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class XORI(rd: Reg, rs1: Reg, imm: Imm) extends OP   {
    def show = s"XORI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 ^ imm($imm) "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.regs(rs1) ^ imm), m.pc + Uint(4))
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} ^ $imm = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class  ORI(rd: Reg, rs1: Reg, imm: Imm) extends OP   {
    def show = s" ORI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 | imm($imm) "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, m.regs(rs1) | imm), m.pc + Uint(4))
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} | $imm = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }


  case class SLLI(rd: Reg, rs1: Reg, imm: Imm) extends OP   {
    def show = s"SLLI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 << imm($imm)[0:4]"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, Uint(m.regs(rs1).toInt >> (imm &  ui"31").toInt)), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} >> r${imm}[0:4] = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class SRLI(rd: Reg, rs1: Reg, imm: Imm) extends OP   {
    def show = s"SRLI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 << imm($imm)[0:4]"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, Uint(m.regs(rs1).toInt >> (imm &  ui"31").toInt)), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} >> r${imm}[0:4] = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class SRAI(rd: Reg, rs1: Reg, imm: Imm) extends OP   {
    def show = s"SRAI   $rd, $rs1, $imm  \t;; r$rd <- r$rs1 << imm($imm)[0:4]"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, Uint(m.regs(rs1).toInt >> (imm &  ui"31").toInt)), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} >> r${imm}[0:4] = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }


  case class SLL(rd: Reg, rs1: Reg, rs2: Reg) extends OP    {
    def show = s"SLL    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 << r$rs2[0:4] "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, Uint(m.regs(rs1).toInt << (m.regs(rs2) &  ui"31").toInt)), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} << r${hs(m.regs(rs2))}[0:4] = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class SRL(rd: Reg, rs1: Reg, rs2: Reg) extends OP    {
    def show = s"SRL    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 >>> r$rs2[0:4] "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, Uint(m.regs(rs1).toInt >>> (m.regs(rs2) &  ui"31").toInt)), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} >>> r${hs(m.regs(rs2))}[0:4] = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class SRA(rd: Reg, rs1: Reg, rs2: Reg) extends OP    {
    def show = s"SRA    $rd, $rs1, $rs2  \t;; r$rd <- r$rs1 >> r$rs2[0:4] "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, Uint(m.regs(rs1).toInt >> (m.regs(rs2) &  ui"31").toInt)), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} >> r${hs(m.regs(rs2))}[0:4] = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }


  case class SLTI(rd: Reg, rs1: Reg, imm: Imm) extends OP   {
    def show = s"SLTI   $rd, $rs1, $imm  \t;; r$rd <- (r$rs1 > imm($imm)) ? 1 : 0"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, if(m.regs(rs1).toInt > imm.toInt) ui"0" else ui"1"), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} > ${imm} = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class SLTIU(rd: Reg, rs1: Reg, imm: Imm) extends OP  {
    def show = s"SLTIU  $rd, $rs1, $imm  \t;; r$rd <- (r$rs1 > imm($imm)) ? 1 : 0"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, if(m.regs(rs1) > imm) ui"0" else ui"1"), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} > ${imm} = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class SLT(rd: Reg, rs1: Reg, rs2: Reg) extends OP    {
    def show = s"SLT    $rd, $rs1, $rs2  \t;; r$rd <- (r$rs1 > r$rs2) ? 1 : 0"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, if(m.regs(rs1).toInt > m.regs(rs2).toInt) ui"0" else ui"1"), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} > r${hs(m.regs(rs2))} = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class SLTU(rd: Reg, rs1: Reg, rs2: Reg) extends OP   {
    def show = s"SLTU   $rd, $rs1, $rs2  \t;; r$rd <- (r$rs1 > r$rs2) ? 1 : 0"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState(m.mem, m.regs.updated(rd, if(m.regs(rs1) > m.regs(rs2)) ui"0" else ui"1"), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.regs(rs1))} > r${hs(m.regs(rs2))} = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }


  case class LUI(rd: Reg, imm: Imm) extends OP              {
    def show = s"LUI    $rd, $imm        \t;; r$rd <- imm($imm) << 12 "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState( m.mem, m.regs.updated(rd, imm << 12), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${imm} << 12 = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }
  case class AUIPC(rd: Reg, imm: Imm) extends OP            {
    def show = s"AUIPC  $rd, $imm        \t;; r$rd <- PC + (imm($imm) << 12) "
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState( m.mem, m.regs.updated(rd, m.pc << 12), m.pc + ui"4")
      val s =
        s"r${rd} changed from ${hs(m.regs(rd))} to r${hs(m.pc)} << 12 = ${hs(next.regs(rd))}" ++
          s"\nPC changed from ${hs(m.pc)} to ${hs(next.pc)}"
      Right((next, show ++ "\n" ++ s))
    }
  }

  case class SW(rs2: Reg, rs1: Reg, offset: Imm) extends OP {
    def show = s"SW     $rs2, $offset($rs2) ;; MEM[r$rs1 + $offset] <- r$rs2"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      ???
    }
  }
  case class LW(rd: Reg, rs1: Reg, offset: Imm) extends OP  {
    def show = s"LW     $rd,  $offset($rs1) ;; r$rd <- MEM[r$rs1 + $offset]"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      ???
    }
  }
  case object DONE extends OP {
    def show = s"we done!"
    def run(m: MachineState): Either[String,(MachineState, String)] = {
      val next = MachineState( m.mem, m.regs, Uint(0xF01D1EF7))
      val s = "we done!"
      Right((next, show ++ "\n" ++ s))
    }
  }
}
