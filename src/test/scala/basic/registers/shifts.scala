package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

import implicitImm._

class RegisterShiftTests extends FlatSpec with Matchers {

  val timeout = 300

  val initRegs = Map(
    x0 -> Uint(0),
    x1 -> Uint(1),
    x2 -> Uint(2),
    x3 -> Uint(3),
    x4 -> Uint(7),
    x5 -> Uint(10),
    x6 -> Uint(12),
    x7 -> Uint(18),
    x8 -> Uint(31),
    x9 -> Uint(51),
    x10 -> Uint(100),

    x11 -> Uint(0xFFFFFFFF),
    x12 -> Uint(0xFFFFFFCE),
    x13 -> Uint(0xFFFFFFAA),
    x14 -> Uint(0xFFFFFE),
    x15 -> Uint(0x8FFFFFFE),
    x16 -> Uint(0x7FFFFFFE),
    x17 -> Uint(0x7FFFFFFF),
    x18 -> Uint(0x7FFFFFAA),

    x31 -> Uint(0),
    )

  val initMem = Map[Addr, Word]()
  val initMachine = MachineState(initMem, initRegs)

  it should "be able to SLL numbers" in {

    val program = List(
      SLL(x31, x1,  x1),
      SLL(x31, x2,  x2),
      SLL(x31, x3,  x3),
      SLL(x31, x4,  x4),
      SLL(x31, x5,  x5),
      SLL(x31, x6,  x6),
      SLL(x31, x7,  x7),
      SLL(x31, x8,  x8),
      SLL(x31, x9,  x9),
      SLL(x31, x10, x10),

      SLL(x31, x1,  x11),
      SLL(x31, x2,  x12),
      SLL(x31, x3,  x13),
      SLL(x31, x4,  x14),
      SLL(x31, x5,  x15),
      SLL(x31, x6,  x16),
      SLL(x31, x7,  x17),
      SLL(x31, x8,  x18),

      SLL(x1, x1, x1),
      SLL(x1, x1, x1),
      SLL(x1, x1, x1),
      SLL(x1, x1, x1),
      SLL(x1, x1, x1),
      SLL(x1, x1, x1),
      SLL(x1, x1, x1),

      SLL(x1, x16, x1),
      SLL(x1, x16, x1),
      SLL(x1, x16, x1),
      SLL(x1, x16, x1),
      SLL(x1, x16, x1),
      SLL(x1, x16, x1),
      SLL(x1, x16, x1),

      SLL(x1, x11, x1),
      SLL(x1, x11, x1),
      SLL(x1, x11, x1),
      SLL(x1, x11, x1),
      SLL(x1, x11, x1),
      SLL(x1, x11, x1),
      SLL(x1, x11, x1),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to SRL numbers" in {

    val program = List(
      SRL(x31, x1,  x1),
      SRL(x31, x2,  x2),
      SRL(x31, x3,  x3),
      SRL(x31, x4,  x4),
      SRL(x31, x5,  x5),
      SRL(x31, x6,  x6),
      SRL(x31, x7,  x7),
      SRL(x31, x8,  x8),
      SRL(x31, x9,  x9),
      SRL(x31, x10, x10),

      SRL(x31, x1,  x11),
      SRL(x31, x2,  x12),
      SRL(x31, x3,  x13),
      SRL(x31, x4,  x14),
      SRL(x31, x5,  x15),
      SRL(x31, x6,  x16),
      SRL(x31, x7,  x17),
      SRL(x31, x8,  x18),

      SRL(x1, x1, x1),
      SRL(x1, x1, x1),
      SRL(x1, x1, x1),
      SRL(x1, x1, x1),
      SRL(x1, x1, x1),
      SRL(x1, x1, x1),
      SRL(x1, x1, x1),

      SRL(x1, x16, x1),
      SRL(x1, x16, x1),
      SRL(x1, x16, x1),
      SRL(x1, x16, x1),
      SRL(x1, x16, x1),
      SRL(x1, x16, x1),
      SRL(x1, x16, x1),

      SRL(x1, x11, x1),
      SRL(x1, x11, x1),
      SRL(x1, x11, x1),
      SRL(x1, x11, x1),
      SRL(x1, x11, x1),
      SRL(x1, x11, x1),
      SRL(x1, x11, x1),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to SRA numbers" in {

    val program = List(
      SRA(x31, x1,  x1),
      SRA(x31, x2,  x2),
      SRA(x31, x3,  x3),
      SRA(x31, x4,  x4),
      SRA(x31, x5,  x5),
      SRA(x31, x6,  x6),
      SRA(x31, x7,  x7),
      SRA(x31, x8,  x8),
      SRA(x31, x9,  x9),
      SRA(x31, x10, x10),

      SRA(x31, x1,  x11),
      SRA(x31, x2,  x12),
      SRA(x31, x3,  x13),
      SRA(x31, x4,  x14),
      SRA(x31, x5,  x15),
      SRA(x31, x6,  x16),
      SRA(x31, x7,  x17),
      SRA(x31, x8,  x18),

      SRA(x1, x1, x1),
      SRA(x1, x1, x1),
      SRA(x1, x1, x1),
      SRA(x1, x1, x1),
      SRA(x1, x1, x1),
      SRA(x1, x1, x1),
      SRA(x1, x1, x1),

      SRA(x1, x16, x1),
      SRA(x1, x16, x1),
      SRA(x1, x16, x1),
      SRA(x1, x16, x1),
      SRA(x1, x16, x1),
      SRA(x1, x16, x1),
      SRA(x1, x16, x1),

      SRA(x1, x11, x1),
      SRA(x1, x11, x1),
      SRA(x1, x11, x1),
      SRA(x1, x11, x1),
      SRA(x1, x11, x1),
      SRA(x1, x11, x1),
      SRA(x1, x11, x1),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
}
