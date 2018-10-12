package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

class ImmidiateComparisonsTests extends FlatSpec with Matchers {

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

  it should "be able to compare immediates signed" in {

    val program = List(
      SLTI(x31, x1, 0),
      SLTI(x31, x1, 1),
      SLTI(x31, x1, 2),
      SLTI(x31, x1, -1),

      SLTI(x31, x16, 0),
      SLTI(x31, x16, 1),
      SLTI(x31, x16, 2),
      SLTI(x31, x16, -1),
      SLTI(x31, x16, -100),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to compare immediates unsigned" in {

    val program = List(
      SLTIU(x31, x1, 0),
      SLTIU(x31, x1, 1),
      SLTIU(x31, x1, 2),
      SLTIU(x31, x1, -1),

      SLTIU(x31, x16, 0),
      SLTIU(x31, x16, 1),
      SLTIU(x31, x16, 2),
      SLTIU(x31, x16, -1),
      SLTIU(x31, x16, -100),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

}
