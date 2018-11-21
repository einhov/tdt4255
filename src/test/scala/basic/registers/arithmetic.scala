package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

import implicitImm._

class RegisterArithmeticTests extends FlatSpec with Matchers {

  val timeout = 300

  val initRegs = Map(
    x0 -> Uint(0),
    x1 -> Uint(1),
    x2 -> Uint(2),
    x3 -> Uint(3),
    x4 -> Uint(4),
    x5 -> Uint(0xFF),
    x6 -> Uint(0xFFFF),
    x7 -> Uint(123456),
    x8 -> Uint(7),

    x11 -> Uint(0xFFFFFFFF),
    x12 -> Uint(0xFFFFFFCE),
    x13 -> Uint(0xFFFFFFAA),
    x14 -> Uint(0xFFFFFE),
    x15 -> Uint(0x8FFFFFFE),
    x16 -> Uint(0x7FFFFFFE),
    x17 -> Uint(0x7FFFFFFF),
    x18 -> Uint(0x7FFFFFAA),

    x20 -> Uint(0),
    x21 -> Uint(0),
    x22 -> Uint(1),
    x23 -> Uint(4),
    x24 -> Uint(4),
    x25 -> Uint(4),
    x26 -> Uint(4),
    x27 -> Uint(4),
    x28 -> Uint(4),
    )

  val initMem = Map[Addr, Word]()
  val initMachine = MachineState(initMem, initRegs)

  it should "be able to add numbers" in {

    val program = List(
      ADD(x21, x1, x2),
      ADD(x22, x3, x4),
      ADD(x23, x1, x1),
      ADD(x24, x2, x2),
      ADD(x25, x4, x4),
      ADD(x26, x5, x5),
      ADD(x27, x6, x6),
      ADD(x28, x7, x8),

      ADD(x21, x1, x8),
      ADD(x22, x2, x7),
      ADD(x23, x3, x6),
      ADD(x24, x4, x5),
      ADD(x25, x5, x4),
      ADD(x26, x6, x3),
      ADD(x27, x7, x2),
      ADD(x28, x8, x1),

      ADD(x21, x11, x1),
      ADD(x22, x12, x2),
      ADD(x23, x13, x3),
      ADD(x24, x14, x4),
      ADD(x25, x15, x5),
      ADD(x26, x16, x6),
      ADD(x27, x17, x7),
      ADD(x28, x18, x8),

      ADD(x1, x1, x1),
      ADD(x1, x1, x1),
      ADD(x1, x1, x1),
      ADD(x1, x1, x1),
      ADD(x1, x1, x1),
      ADD(x1, x1, x1),
      ADD(x1, x1, x1),
      ADD(x1, x1, x1),
      ADD(x1, x1, x1),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to subtract numbers" in {

    val program = List(
      SUB(x21, x1, x2),
      SUB(x22, x3, x4),
      SUB(x23, x1, x1),
      SUB(x24, x2, x2),
      SUB(x25, x4, x4),
      SUB(x26, x5, x5),
      SUB(x27, x6, x6),
      SUB(x28, x7, x8),

      SUB(x21, x1, x8),
      SUB(x22, x2, x7),
      SUB(x23, x3, x6),
      SUB(x24, x4, x5),
      SUB(x25, x5, x4),
      SUB(x26, x6, x3),
      SUB(x27, x7, x2),
      SUB(x28, x8, x1),

      SUB(x21, x11, x1),
      SUB(x22, x12, x2),
      SUB(x23, x13, x3),
      SUB(x24, x14, x4),
      SUB(x25, x15, x5),
      SUB(x26, x16, x6),
      SUB(x27, x17, x7),
      SUB(x28, x18, x8),

      SUB(x1, x1, x1),
      SUB(x1, x1, x1),
      SUB(x1, x1, x1),
      SUB(x1, x1, x1),
      SUB(x1, x1, x1),
      SUB(x1, x1, x1),
      SUB(x1, x1, x1),
      SUB(x1, x1, x1),
      SUB(x1, x1, x1),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
  it should "be able to logical OR numbers" in {

    val program = List(
      OR(x21, x1, x2),
      OR(x22, x3, x4),
      OR(x23, x1, x1),
      OR(x24, x2, x2),
      OR(x25, x4, x4),
      OR(x26, x5, x5),
      OR(x27, x6, x6),
      OR(x28, x7, x8),

      OR(x21, x1, x8),
      OR(x22, x2, x7),
      OR(x23, x3, x6),
      OR(x24, x4, x5),
      OR(x25, x5, x4),
      OR(x26, x6, x3),
      OR(x27, x7, x2),
      OR(x28, x8, x1),

      OR(x21, x11, x1),
      OR(x22, x12, x2),
      OR(x23, x13, x3),
      OR(x24, x14, x4),
      OR(x25, x15, x5),
      OR(x26, x16, x6),
      OR(x27, x17, x7),
      OR(x28, x18, x8),

      OR(x1, x1, x1),
      OR(x1, x1, x1),
      OR(x1, x1, x1),
      OR(x1, x1, x1),
      OR(x1, x1, x1),
      OR(x1, x1, x1),
      OR(x1, x1, x1),
      OR(x1, x1, x1),
      OR(x1, x1, x1),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
  it should "be able to logical AND numbers" in {

    val program = List(
      AND(x21, x1, x2),
      AND(x22, x3, x4),
      AND(x23, x1, x1),
      AND(x24, x2, x2),
      AND(x25, x4, x4),
      AND(x26, x5, x5),
      AND(x27, x6, x6),
      AND(x28, x7, x8),

      AND(x21, x1, x8),
      AND(x22, x2, x7),
      AND(x23, x3, x6),
      AND(x24, x4, x5),
      AND(x25, x5, x4),
      AND(x26, x6, x3),
      AND(x27, x7, x2),
      AND(x28, x8, x1),

      AND(x21, x11, x1),
      AND(x22, x12, x2),
      AND(x23, x13, x3),
      AND(x24, x14, x4),
      AND(x25, x15, x5),
      AND(x26, x16, x6),
      AND(x27, x17, x7),
      AND(x28, x18, x8),

      AND(x1, x1, x1),
      AND(x1, x1, x1),
      AND(x1, x1, x1),
      AND(x1, x1, x1),
      AND(x1, x1, x1),
      AND(x1, x1, x1),
      AND(x1, x1, x1),
      AND(x1, x1, x1),
      AND(x1, x1, x1),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to logical XOR numbers" in {

    val program = List(
      XOR(x21, x1, x2),
      XOR(x22, x3, x4),
      XOR(x23, x1, x1),
      XOR(x24, x2, x2),
      XOR(x25, x4, x4),
      XOR(x26, x5, x5),
      XOR(x27, x6, x6),
      XOR(x28, x7, x8),

      XOR(x21, x1, x8),
      XOR(x22, x2, x7),
      XOR(x23, x3, x6),
      XOR(x24, x4, x5),
      XOR(x25, x5, x4),
      XOR(x26, x6, x3),
      XOR(x27, x7, x2),
      XOR(x28, x8, x1),

      XOR(x21, x11, x1),
      XOR(x22, x12, x2),
      XOR(x23, x13, x3),
      XOR(x24, x14, x4),
      XOR(x25, x15, x5),
      XOR(x26, x16, x6),
      XOR(x27, x17, x7),
      XOR(x28, x18, x8),

      XOR(x1, x1, x1),
      XOR(x1, x1, x1),
      XOR(x1, x1, x1),
      XOR(x1, x1, x1),
      XOR(x1, x1, x1),
      XOR(x1, x1, x1),
      XOR(x1, x1, x1),
      XOR(x1, x1, x1),
      XOR(x1, x1, x1),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
}
