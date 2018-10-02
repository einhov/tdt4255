
package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

class RegisterComparisonTests extends FlatSpec with Matchers {

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

  it should "be able to do signed comparison of numbers" in {

    val program = List(
      SLT(x21, x1, x2),
      SLT(x22, x3, x4),
      SLT(x23, x1, x1),
      SLT(x24, x2, x2),
      SLT(x25, x4, x4),
      SLT(x26, x5, x5),
      SLT(x27, x6, x6),
      SLT(x28, x7, x8),

      SLT(x21, x1, x8),
      SLT(x22, x2, x7),
      SLT(x23, x3, x6),
      SLT(x24, x4, x5),
      SLT(x25, x5, x4),
      SLT(x26, x6, x3),
      SLT(x27, x7, x2),
      SLT(x28, x8, x1),

      SLT(x21, x11, x1),
      SLT(x22, x12, x2),
      SLT(x23, x13, x3),
      SLT(x24, x14, x4),
      SLT(x25, x15, x5),
      SLT(x26, x16, x6),
      SLT(x27, x17, x7),
      SLT(x28, x18, x8),

      SLT(x21, x11, x18),
      SLT(x22, x12, x17),
      SLT(x23, x13, x16),
      SLT(x24, x14, x15),
      SLT(x25, x15, x14),
      SLT(x26, x16, x13),
      SLT(x27, x17, x12),
      SLT(x28, x18, x11),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to do unsigned comparison of numbers" in {

    val program = List(
      SLTU(x21, x1, x2),
      SLTU(x22, x3, x4),
      SLTU(x23, x1, x1),
      SLTU(x24, x2, x2),
      SLTU(x25, x4, x4),
      SLTU(x26, x5, x5),
      SLTU(x27, x6, x6),
      SLTU(x28, x7, x8),

      SLTU(x21, x1, x8),
      SLTU(x22, x2, x7),
      SLTU(x23, x3, x6),
      SLTU(x24, x4, x5),
      SLTU(x25, x5, x4),
      SLTU(x26, x6, x3),
      SLTU(x27, x7, x2),
      SLTU(x28, x8, x1),

      SLTU(x21, x11, x1),
      SLTU(x22, x12, x2),
      SLTU(x23, x13, x3),
      SLTU(x24, x14, x4),
      SLTU(x25, x15, x5),
      SLTU(x26, x16, x6),
      SLTU(x27, x17, x7),
      SLTU(x28, x18, x8),

      SLTU(x21, x11, x18),
      SLTU(x22, x12, x17),
      SLTU(x23, x13, x16),
      SLTU(x24, x14, x15),
      SLTU(x25, x15, x14),
      SLTU(x26, x16, x13),
      SLTU(x27, x17, x12),
      SLTU(x28, x18, x11),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
}
