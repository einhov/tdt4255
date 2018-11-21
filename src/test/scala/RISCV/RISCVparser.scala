package Ov1
import atto._, Atto._
import cats.implicits._
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import fileUtils._

object RISCVparser {

  def space: Parser[Unit] = (many(whitespace).void)

  def sep: Parser[Unit] = (space ~ char(',') ~ space).void

  def reg: Parser[Int] =
    takeWhile(x => (x != ',' && x != ')')).map(lookupReg).flatMap(x => if(x==32) err("") else ok(x))

  def label: Parser[String] =
    takeWhile(c => c != ' ' && c != ')')

  def reg3: Parser[(Int, Int, Int)] = for {
    rd  <- space ~> reg <~ sep
    rs1 <- reg <~ sep
    rs2 <- reg
  } yield((rd, rs1, rs2))

  def relImm: Parser[Immediate] = {
    int.map{x => Immediate(x)} |
      label.map{x => Immediate.pcrel(x)}
  }

  def imm: Parser[Immediate] = {
    int.map{x => Immediate(x)} |
      (string("%hi") ~> parens(label).map(Immediate.upper)) |
      (string("%lo") ~> parens(label).map(Immediate.lower)) |
      label.map{x => Immediate(x)}
  }


  def reg2imm: Parser[(Int, Int, Immediate)] = for {
    rd  <- space ~> reg <~ sep
    rs1 <-          reg <~ sep
    i   <-          imm
  } yield ((rd, rs1, i))

  def reg2immRel: Parser[(Int, Int, Immediate)] = for {
    rd  <- space ~> reg <~ sep
    rs1 <-          reg <~ sep
    i   <-          relImm
  } yield ((rd, rs1, i))

  def regImmReg: Parser[(Int, Immediate, Int)] = for {
    r1  <- space ~> reg <~ sep
    i   <- imm
    r2  <- parens(reg)
  } yield ((r1, i, r2))

  def regImm: Parser[(Int, Immediate)] = for {
    rd <- space ~> reg <~ sep
    i  <- imm
  } yield ((rd, i))

  def regImmRel: Parser[(Int, Immediate)] = for {
    rd <- space ~> reg <~ sep
    i  <- relImm
  } yield ((rd, i))


  def parseLookup(s: String): Parser[asmOP] = space *> {

    import implicitImm._
    s match {
      case "beq" =>  reg2immRel.map(Function.tupled(BEQ))
      case "bne" =>  reg2immRel.map(Function.tupled(BNE))
      case "blt" =>  reg2immRel.map(Function.tupled(BLT))
      case "bge" =>  reg2immRel.map(Function.tupled(BGE))
      case "bltu" => reg2immRel.map(Function.tupled(BLTU))
      case "bgeu" => reg2immRel.map(Function.tupled(BGEU))

        // yes we really did need all of these fuckers to exist as pseudo ops.
        // what do you mean we could use macros?
        // dude if we were intelligent enough for that we wouldnt be writing asm.
      case "ble" =>  reg2immRel.map{ case(rs1, rs2, imm) => BGE(rs2, rs1, imm) }
      case "bgt" =>  reg2immRel.map{ case(rs1, rs2, imm) => BLT(rs2, rs1, imm) }
      case "bleu" => reg2immRel.map{ case(rs1, rs2, imm) => BGEU(rs2, rs1, imm) }
      case "bgtu" => reg2immRel.map{ case(rs1, rs2, imm) => BLTU(rs2, rs1, imm) }
      case "bnez" => regImmRel.map{  case(rs1,      imm) => BNE(rs1, 0, imm) }


      case "add" =>  reg3.map(Function.tupled(ADD))
      case "sub" =>  reg3.map(Function.tupled(SUB))
      case "or"  =>  reg3.map(Function.tupled(OR))
      case "xor" =>  reg3.map(Function.tupled(XOR))
      case "and" =>  reg3.map(Function.tupled(AND))
      case "sll" =>  reg3.map(Function.tupled(SLL))
      case "srl" =>  reg3.map(Function.tupled(SRL))
      case "sra" =>  reg3.map(Function.tupled(SRA))
      case "slt" =>  reg3.map(Function.tupled(SLT))
      case "sltu" => reg3.map(Function.tupled(SLTU))

      case "addi"  => reg2imm.map(Function.tupled(ADDI))
      case "andi"  => reg2imm.map(Function.tupled(ANDI))
      case "xori"  => reg2imm.map(Function.tupled(XORI))
      case "ori"   => reg2imm.map(Function.tupled(ORI))
      case "slli"  => reg2imm.map(Function.tupled(SLLI))
      case "srli"  => reg2imm.map(Function.tupled(SRLI))
      case "srai"  => reg2imm.map(Function.tupled(SRAI))
      case "slti"  => reg2imm.map(Function.tupled(SLTI))
      case "sltiu" => reg2imm.map(Function.tupled(SLTIU))

      case "jalr" => reg2imm.map(Function.tupled(JALR))

      case "sw" => regImmReg.map{ case(rs2, offset, rs1) => SW(rs2, rs1, offset) }
      case "lw" => regImmReg.map{ case(rd,  offset, rs1) => LW(rd,  rs1, offset) }

      case "lui"   => regImm.map(Function.tupled(LUI))
      case "auipc" => regImm.map(Function.tupled(AUIPC))
      case "jal"   => regImmRel.map(Function.tupled(JAL))

      case "li"   => regImm.map{ case(rd, imm) => ADDI(rd, 0, imm) }
      case "call" => space *> imm.map(imm => CALL(imm))
      case "jr"   => reg.map(r => JR(r))
      case "mv"   => (reg ~ sep ~ reg).map{ case((rd, _), rs) => ADD(rd, 0, rs)}
      case "ret"  => ok(RET)
      case "j"    => relImm.map(J)

        // TODO figure out how to better represent this.
        // Is it possible to "preload" a parser?
      case (s: String) => {
        if(s.last == ':')
          ok(LABEL(s.reverse.tail.reverse))
        else
          err("")
      }
    }
  }


  def parseOp: Parser[asmOP] = space *> label.flatMap(parseLookup)

  def parseLabelDest: Parser[asmOP] = space *> (takeWhile (_ != ':') ~ char(':'))
    .map(x => LABEL(x._1))


  // should be to Option[Int] but dunno how to apply it to a parser xD xD
  def lookupReg(s: String): Int = {
    s match {
      case "x0"  =>  0

      case "x1"  => 1
      case "x2"  => 2
      case "x3"  => 3
      case "x4"  => 4
      case "x5"  => 5
      case "x6"  => 6
      case "x7"  => 7
      case "x8"  => 8
      case "x9"  => 9

      case "x10" => 10
      case "x11" => 11
      case "x12" => 12
      case "x13" => 13
      case "x14" => 14
      case "x15" => 15
      case "x16" => 16
      case "x17" => 17
      case "x18" => 18
      case "x19" => 19

      case "x20" => 20
      case "x21" => 21
      case "x22" => 22
      case "x23" => 23
      case "x24" => 24
      case "x25" => 25
      case "x26" => 26
      case "x27" => 27
      case "x28" => 28
      case "x29" => 29

      case "x30" => 30
      case "x31" => 31


      case "zero" => 0
      case "ra"   => 1
      case "sp"   => 2
      case "gp"   => 3
      case "tp"   => 4
      case "t0"   => 5
      case "t1"   => 6
      case "t2"   => 7
      case "s0"   => 8
      case "fp"   => 8
      case "s1"   => 9

      case "a0"   => 10
      case "a1"   => 11
      case "a2"   => 12
      case "a3"   => 13
      case "a4"   => 14
      case "a5"   => 15
      case "a6"   => 16
      case "a7"   => 17
      case "s2"   => 18
      case "s3"   => 19

      case "s4"   => 20
      case "s5"   => 21
      case "s6"   => 22
      case "s7"   => 23
      case "s8"   => 24
      case "s9"   => 25
      case "s10"  => 26
      case "s11"  => 27
      case "t3"   => 28
      case "t4"   => 29

      case "t5"   => 30
      case "t6"   => 31
      case _ => 32
    }
  }
}
