package Ov1
import atto._, Atto._
import cats.Functor
import cats.implicits._
import utilz._

import org.scalatest.{Matchers, FlatSpec}
import spire.syntax.literals.radix._

import scala.util.Random.shuffle


import RISCVutils._
import RISCVOPS._

import spire.math.{UInt => Uint}

object assembler {


  def setField(firstBit: Int, size: Int, field: Uint): Uint => Uint = instruction => {
    val shiftedField = field << firstBit
    val mask  = Uint(1 << size) - Uint(1)
    val shiftedMask = (mask << firstBit)
    val masked = ~((~instruction) | shiftedMask)

    if(log2(field.toInt) > size){
      println(s"Attempted to set field too large for mask.")
    }


    val ret = shiftedField | masked
    ret
  }


  def getSubField(firstBit: Int, size: Int): Uint => Uint = word => {
    val bitsLeft = 32 - firstBit
    val bitsRight = 32 - size
    val leftShifted = word << bitsLeft
    val rightShifted = leftShifted >> bitsRight
    rightShifted
  }


  /**
    Splits the immediate value into fields given by points.
    The order of points is important!
    points is of type idx, size
    */
  def applyImmediate(immediateBits: Int, immediate: Int, points: List[(Int, Int)]): Uint => Uint = instruction => {

    def go(instruction: Uint, immediateIndex: Int, points: List[(Int,Int)]): Uint = points match {
      case h :: t => {
        val (immFirstBit, size) = h
        val firstBit = (immFirstBit - size) + 1
        val immSubField = getSubField(immediateIndex, size)(Uint(immediate))
        val nextImmIndex = immediateIndex - size
        val nextInstruction = setField(firstBit, size, immSubField)(instruction)
        go(nextInstruction, nextImmIndex, points.tail)
      }
      case _ => {
        instruction
      }
    }

    go(instruction, immediateBits, points)

  }


  def setItypeImmediate(instruction: Uint, immediate: Int): Uint = {
    val points = List((31, 12))
    val huh = applyImmediate(12, immediate, points)(instruction)
    huh
  }

  def setStypeImmediate(instruction: Uint, immediate: Int): Uint = {
    val points = List((31, 7), (11, 5))
    val huh = applyImmediate(12, immediate, points)(instruction)
    huh
  }

  def setBtypeImmediate(instruction: Uint, immediate: Int): Uint = {
    val points = List((31, 1), (7, 1), (30, 6), (11, 4))
    val huh = applyImmediate(13, immediate, points)(instruction)
    huh
  }

  // TODO doubleCheck
  def setUtypeImmediate(instruction: Uint, immediate: Int): Uint = {
    val points = List((31, 20))
    val huh = applyImmediate(20, immediate, points)(instruction)
    huh
  }

  def setJtypeImmediate(instruction: Uint, immediate: Int): Uint = {
    val points = List((31, 1), (19, 8), (20, 1), (30, 10))
    val huh = applyImmediate(21, immediate, points)(instruction)
    huh
  }

  def setShiftTypeImmediate(instruction: Uint, immediate: Int): Uint = {
    val points = List((24, 5))
    val huh = applyImmediate(5, immediate, points)(instruction)
    huh
  }

  def setOpCode(opcode: Int): Uint => Uint = instruction => {
    val huh = setField(0, 7, Uint(opcode))(instruction)
    huh
  }

  def setFunct7(funct7: Int): Uint => Uint = instruction => {
    val huh = setField(25, 7, Uint(funct7))(instruction)
    huh
  }

  def setFunct3(funct3: Int): Uint => Uint = instruction => {
    val huh = setField(12, 3, Uint(funct3))(instruction)
    huh
  }

  def setRs1(rs1: Int): Uint => Uint = instruction => {
    val huh = setField(15, 5, Uint(rs1))(instruction)
    huh
  }

  def setRs2(rs2: Int): Uint => Uint = instruction => {
    val huh = setField(20, 5, Uint(rs2))(instruction)
    huh
  }

  def setRd(rd: Int): Uint => Uint = instruction => {
    val huh = setField(7, 5, Uint(rd))(instruction)
    huh
  }




  def assembleRType(op: OP): Uint => Uint = {

    def setRegs(rs1: Int, rs2: Int, rd: Int) = setRs1(rs1) andThen setRs2(rs2) andThen setRd(rd)

    op match {
      case ADD(rd, rs1, rs2)  => setOpCode(x2"0110011") andThen setFunct7(x2"0000000") andThen setFunct3(x2"000") andThen setRegs(rs1, rs2, rd)
      case AND(rd, rs1, rs2)  => setOpCode(x2"0110011") andThen setFunct7(x2"0000000") andThen setFunct3(x2"111") andThen setRegs(rs1, rs2, rd)
      case SUB(rd, rs1, rs2)  => setOpCode(x2"0110011") andThen setFunct7(x2"0100000") andThen setFunct3(x2"000") andThen setRegs(rs1, rs2, rd)
      case OR(rd, rs1, rs2)   => setOpCode(x2"0110011") andThen setFunct7(x2"0000000") andThen setFunct3(x2"110") andThen setRegs(rs1, rs2, rd)
      case XOR(rd, rs1, rs2)  => setOpCode(x2"0110011") andThen setFunct7(x2"0000000") andThen setFunct3(x2"100") andThen setRegs(rs1, rs2, rd)
      case SLT(rd, rs1, rs2)  => setOpCode(x2"0110011") andThen setFunct7(x2"0000000") andThen setFunct3(x2"010") andThen setRegs(rs1, rs2, rd)
      case SLTU(rd, rs1, rs2) => setOpCode(x2"0110011") andThen setFunct7(x2"0000000") andThen setFunct3(x2"011") andThen setRegs(rs1, rs2, rd)
      case SRA(rd, rs1, rs2)  => setOpCode(x2"0110011") andThen setFunct7(x2"0100000") andThen setFunct3(x2"101") andThen setRegs(rs1, rs2, rd)
      case SRL(rd, rs1, rs2)  => setOpCode(x2"0110011") andThen setFunct7(x2"0000000") andThen setFunct3(x2"101") andThen setRegs(rs1, rs2, rd)
      case SLL(rd, rs1, rs2)  => setOpCode(x2"0110011") andThen setFunct7(x2"0000000") andThen setFunct3(x2"001") andThen setRegs(rs1, rs2, rd)
      case _ => identity
    }
  }

  def assembleIType(op: OP): Uint => Uint = {
    def setRegs(rs1: Int, rd: Int) = setRs1(rs1) andThen setRd(rd)

    op match {
      case ADDI(rd, rs1, imm)  => setOpCode(x2"0010011") andThen setFunct3(x2"000") andThen setRegs(rs1, rd) andThen assembleImmediates(op)
      case ANDI(rd, rs1, imm)  => setOpCode(x2"0010011") andThen setFunct3(x2"111") andThen setRegs(rs1, rd) andThen assembleImmediates(op)
      case ORI(rd, rs1, imm)   => setOpCode(x2"0010011") andThen setFunct3(x2"110") andThen setRegs(rs1, rd) andThen assembleImmediates(op)
      case XORI(rd, rs1, imm)  => setOpCode(x2"0010011") andThen setFunct3(x2"100") andThen setRegs(rs1, rd) andThen assembleImmediates(op)
      case SLTI(rd, rs1, imm)  => setOpCode(x2"0010011") andThen setFunct3(x2"010") andThen setRegs(rs1, rd) andThen assembleImmediates(op)
      case SLTIU(rd, rs1, imm) => setOpCode(x2"0010011") andThen setFunct3(x2"011") andThen setRegs(rs1, rd) andThen assembleImmediates(op)
      case SRAI(rd, rs1, imm)  => setOpCode(x2"0010011") andThen setFunct3(x2"101") andThen setRegs(rs1, rd) andThen assembleImmediates(op) andThen setFunct7(x2"0100000")
      case SRLI(rd, rs1, imm)  => setOpCode(x2"0010011") andThen setFunct3(x2"101") andThen setRegs(rs1, rd) andThen assembleImmediates(op)
      case SLLI(rd, rs1, imm)  => setOpCode(x2"0010011") andThen setFunct3(x2"001") andThen setRegs(rs1, rd) andThen assembleImmediates(op)

      case LW(rd, rs1, imm)    => setOpCode(x2"0000011") andThen setFunct3(x2"010") andThen setRegs(rs1, rd) andThen assembleImmediates(op)
      case JALR(rd, rs1, imm)  => setOpCode(x2"1100111") andThen setFunct3(x2"000") andThen setRegs(rs1, rd) andThen assembleImmediates(op)
      case _ => identity
    }
  }


  def assembleSType(op: OP): Uint => Uint = {
    def setRegs(rs1: Int, rs2: Int) = setRs1(rs1) andThen setRs2(rs2)

    op match {
      // note rs2 and 1 switched
      case SW(rs2, rs1, imm)  => setOpCode(x2"0100011") andThen setFunct3(x2"010") andThen setRegs(rs1, rs2) andThen assembleImmediates(op)
      case _ => identity
    }
  }

  def assembleBType(op: OP): Uint => Uint = {
    def setRegs(rs1: Int, rs2: Int) = setRs1(rs1) andThen setRs2(rs2)

    op match {
      case BEQ(rs1, rs2, imm)   => setOpCode(x2"1100011") andThen setFunct3(x2"000") andThen setRegs(rs1, rs2) andThen assembleImmediates(op)
      case BNE(rs1, rs2, imm)   => setOpCode(x2"1100011") andThen setFunct3(x2"001") andThen setRegs(rs1, rs2) andThen assembleImmediates(op)
      case BLT(rs1, rs2, imm)   => setOpCode(x2"1100011") andThen setFunct3(x2"100") andThen setRegs(rs1, rs2) andThen assembleImmediates(op)
      case BGE(rs1, rs2, imm)   => setOpCode(x2"1100011") andThen setFunct3(x2"101") andThen setRegs(rs1, rs2) andThen assembleImmediates(op)
      case BLTU(rs1, rs2, imm)  => setOpCode(x2"1100011") andThen setFunct3(x2"110") andThen setRegs(rs1, rs2) andThen assembleImmediates(op)
      case BGEU(rs1, rs2, imm)  => setOpCode(x2"1100011") andThen setFunct3(x2"111") andThen setRegs(rs1, rs2) andThen assembleImmediates(op)
      case _ => identity
    }
  }


  def assembleUType(op: OP): Uint => Uint = {
    op match {
      case LUI(rd, imm)   => setOpCode(x2"0110111") andThen setRd(rd) andThen assembleImmediates(op)
      case AUIPC(rd, imm) => setOpCode(x2"0010111") andThen setRd(rd) andThen assembleImmediates(op)
      case _ => identity
    }
  }


  def assembleJType(op: OP): Uint => Uint = {
    op match {
      case JAL(rd, imm)   => setOpCode(x2"1101111") andThen setRd(rd) andThen assembleImmediates(op)
      case _ => identity
    }
  }


  def assembleImmediates(op: OP): Uint => Uint = {
    op match {
      case BEQ(_, _, imm)   => setBtypeImmediate(_, imm)
      case BNE(_, _, imm)   => setBtypeImmediate(_, imm)
      case BLT(_, _, imm)   => setBtypeImmediate(_, imm)
      case BGE(_, _, imm)   => setBtypeImmediate(_, imm)
      case BLTU(_, _, imm)  => setBtypeImmediate(_, imm)
      case BGEU(_, _, imm)  => setBtypeImmediate(_, imm)

      case JALR(_, _, imm)  => setItypeImmediate(_, imm)
      case JAL(_, imm)      => setJtypeImmediate(_, imm)

      case ADDI(_, _, imm)  => setItypeImmediate(_, imm)
      case ANDI(_, _, imm)  => setItypeImmediate(_, imm)
      case ORI(_, _, imm)   => setItypeImmediate(_, imm)
      case XORI(_, _, imm)  => setItypeImmediate(_, imm)

      case SLLI(_, _, imm)  => setShiftTypeImmediate(_, imm)
      case SRLI(_, _, imm)  => setShiftTypeImmediate(_, imm)
      case SRAI(_, _, imm)  => setShiftTypeImmediate(_, imm)

      case SLTI(_, _, imm)  => setItypeImmediate(_, imm)
      case SLTIU(_, _, imm) => setItypeImmediate(_, imm)

      case LUI(_, imm)      => setUtypeImmediate(_, imm)
      case AUIPC(_, imm)    => setUtypeImmediate(_, imm)

      case SW(_, _, imm)    => setStypeImmediate(_, imm)
      case LW(_, _, imm)    => setItypeImmediate(_, imm)

      case _ => ???
    }
  }


  def assembleProgram(program: RISCVProgram): List[Uint] = {

    def assembleOp(op: OP): Uint = {

      // This is OK since an unmatched assemble returns identity
      // It's not OK in the sense that it subverts typesafety...
      val huh = assembleRType(op) andThen
        assembleIType(op) andThen
        assembleSType(op) andThen
        assembleBType(op) andThen
        assembleUType(op) andThen
        assembleJType(op)
      huh(Uint(0))
    }

    program.instructions.toList.sortBy(_._1).map(x => assembleOp(x._2))
  }
}
