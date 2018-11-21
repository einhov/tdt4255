package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

import implicitImm._

class ImmediateShiftTests extends FlatSpec with Matchers {

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

    )

  val initMem = Map[Addr, Word]()
  val initMachine = MachineState(initMem, initRegs)

  it should "be able to immediate sll numbers" in {

    val program = List(

      SLLI(x31, x1, 1),
      SLLI(x31, x1, 2),
      SLLI(x31, x1, 3),
      SLLI(x31, x1, 7),
      SLLI(x31, x1, 14),
      SLLI(x31, x1, 28),

      SLLI(x31, x11, 1),
      SLLI(x31, x11, 2),
      SLLI(x31, x11, 3),
      SLLI(x31, x11, 7),
      SLLI(x31, x11, 14),
      SLLI(x31, x11, 28),

      SLLI(x31, x16, 1),
      SLLI(x31, x16, 2),
      SLLI(x31, x16, 3),
      SLLI(x31, x16, 7),
      SLLI(x31, x16, 14),
      SLLI(x31, x16, 28),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to immediate srai numbers" in {

    val program = List(

      SRAI(x31, x1, 1),
      SRAI(x31, x1, 2),
      SRAI(x31, x1, 3),
      SRAI(x31, x1, 7),
      SRAI(x31, x1, 14),
      SRAI(x31, x1, 28),

      SRAI(x31, x11, 1),
      SRAI(x31, x11, 2),
      SRAI(x31, x11, 3),
      SRAI(x31, x11, 7),
      SRAI(x31, x11, 14),
      SRAI(x31, x11, 28),

      SRAI(x31, x16, 1),
      SRAI(x31, x16, 2),
      SRAI(x31, x16, 3),
      SRAI(x31, x16, 7),
      SRAI(x31, x16, 14),
      SRAI(x31, x16, 28),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to immediate srli numbers" in {

    val program = List(

      SRLI(x31, x1, 1),
      SRLI(x31, x1, 2),
      SRLI(x31, x1, 3),
      SRLI(x31, x1, 7),
      SRLI(x31, x1, 14),
      SRLI(x31, x1, 28),

      SRLI(x31, x11, 1),
      SRLI(x31, x11, 2),
      SRLI(x31, x11, 3),
      SRLI(x31, x11, 7),
      SRLI(x31, x11, 14),
      SRLI(x31, x11, 28),

      SRLI(x31, x16, 1),
      SRLI(x31, x16, 2),
      SRLI(x31, x16, 3),
      SRLI(x31, x16, 7),
      SRLI(x31, x16, 14),
      SRLI(x31, x16, 28),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
}
