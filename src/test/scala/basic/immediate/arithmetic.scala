package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

import implicitImm._

class ImmediateArithmeticTests extends FlatSpec with Matchers {

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

  it should "be able to add immediate numbers" in {

    val program = List(

      ADDI(x31, x1, 1),
      ADDI(x31, x1, 2),
      ADDI(x31, x1, 3),
      ADDI(x31, x1, 7),
      ADDI(x31, x1, 14),
      ADDI(x31, x1, 28),
      ADDI(x31, x1, 56),
      ADDI(x31, x1, 133),
      ADDI(x31, x1, 258),
      ADDI(x31, x1, 511),

      ADDI(x31, x1, -1),
      ADDI(x31, x1, -3),
      ADDI(x31, x1, -9),
      ADDI(x31, x1, -98),
      ADDI(x31, x1, -231),
      ADDI(x31, x1, -510),

      ADDI(x31, x11, 1),
      ADDI(x31, x11, 2),
      ADDI(x31, x11, 3),
      ADDI(x31, x11, 7),
      ADDI(x31, x11, 14),
      ADDI(x31, x11, 28),
      ADDI(x31, x11, 56),
      ADDI(x31, x11, 133),
      ADDI(x31, x11, 258),
      ADDI(x31, x11, 511),

      ADDI(x31, x11, -1),
      ADDI(x31, x11, -3),
      ADDI(x31, x11, -9),
      ADDI(x31, x11, -98),
      ADDI(x31, x11, -231),
      ADDI(x31, x11, -510),

      ADDI(x31, x16, 1),
      ADDI(x31, x16, 2),
      ADDI(x31, x16, 3),
      ADDI(x31, x16, 7),
      ADDI(x31, x16, 14),
      ADDI(x31, x16, 28),
      ADDI(x31, x16, 56),
      ADDI(x31, x16, 133),
      ADDI(x31, x16, 258),
      ADDI(x31, x16, 511),

      ADDI(x31, x16, -1),
      ADDI(x31, x16, -3),
      ADDI(x31, x16, -9),
      ADDI(x31, x16, -98),
      ADDI(x31, x16, -231),
      ADDI(x31, x16, -510),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to logical and immediate numbers" in {

    val program = List(

      ANDI(x31, x1, 1),
      ANDI(x31, x1, 2),
      ANDI(x31, x1, 3),
      ANDI(x31, x1, 7),
      ANDI(x31, x1, 14),
      ANDI(x31, x1, 28),
      ANDI(x31, x1, 56),
      ANDI(x31, x1, 133),
      ANDI(x31, x1, 258),
      ANDI(x31, x1, 511),

      ANDI(x31, x1, -1),
      ANDI(x31, x1, -3),
      ANDI(x31, x1, -9),
      ANDI(x31, x1, -98),
      ANDI(x31, x1, -231),
      ANDI(x31, x1, -510),

      ANDI(x31, x11, 1),
      ANDI(x31, x11, 2),
      ANDI(x31, x11, 3),
      ANDI(x31, x11, 7),
      ANDI(x31, x11, 14),
      ANDI(x31, x11, 28),
      ANDI(x31, x11, 56),
      ANDI(x31, x11, 133),
      ANDI(x31, x11, 258),
      ANDI(x31, x11, 511),

      ANDI(x31, x11, -1),
      ANDI(x31, x11, -3),
      ANDI(x31, x11, -9),
      ANDI(x31, x11, -98),
      ANDI(x31, x11, -231),
      ANDI(x31, x11, -510),

      ANDI(x31, x16, 1),
      ANDI(x31, x16, 2),
      ANDI(x31, x16, 3),
      ANDI(x31, x16, 7),
      ANDI(x31, x16, 14),
      ANDI(x31, x16, 28),
      ANDI(x31, x16, 56),
      ANDI(x31, x16, 133),
      ANDI(x31, x16, 258),
      ANDI(x31, x16, 511),

      ANDI(x31, x16, -1),
      ANDI(x31, x16, -3),
      ANDI(x31, x16, -9),
      ANDI(x31, x16, -98),
      ANDI(x31, x16, -231),
      ANDI(x31, x16, -510),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }

  it should "be able to XORI immediate numbers" in {

    val program = List(

      XORI(x31, x1, 1),
      XORI(x31, x1, 2),
      XORI(x31, x1, 3),
      XORI(x31, x1, 7),
      XORI(x31, x1, 14),
      XORI(x31, x1, 28),
      XORI(x31, x1, 56),
      XORI(x31, x1, 133),
      XORI(x31, x1, 258),
      XORI(x31, x1, 511),

      XORI(x31, x1, -1),
      XORI(x31, x1, -3),
      XORI(x31, x1, -9),
      XORI(x31, x1, -98),
      XORI(x31, x1, -231),
      XORI(x31, x1, -510),

      XORI(x31, x11, 1),
      XORI(x31, x11, 2),
      XORI(x31, x11, 3),
      XORI(x31, x11, 7),
      XORI(x31, x11, 14),
      XORI(x31, x11, 28),
      XORI(x31, x11, 56),
      XORI(x31, x11, 133),
      XORI(x31, x11, 258),
      XORI(x31, x11, 511),

      XORI(x31, x11, -1),
      XORI(x31, x11, -3),
      XORI(x31, x11, -9),
      XORI(x31, x11, -98),
      XORI(x31, x11, -231),
      XORI(x31, x11, -510),

      XORI(x31, x16, 1),
      XORI(x31, x16, 2),
      XORI(x31, x16, 3),
      XORI(x31, x16, 7),
      XORI(x31, x16, 14),
      XORI(x31, x16, 28),
      XORI(x31, x16, 56),
      XORI(x31, x16, 133),
      XORI(x31, x16, 258),
      XORI(x31, x16, 511),

      XORI(x31, x16, -1),
      XORI(x31, x16, -3),
      XORI(x31, x16, -9),
      XORI(x31, x16, -98),
      XORI(x31, x16, -231),
      XORI(x31, x16, -510),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }


  it should "be able to logical or immediate numbers" in {

    val program = List(

      ORI(x31, x1, 1),
      ORI(x31, x1, 2),
      ORI(x31, x1, 3),
      ORI(x31, x1, 7),
      ORI(x31, x1, 14),
      ORI(x31, x1, 28),
      ORI(x31, x1, 56),
      ORI(x31, x1, 133),
      ORI(x31, x1, 258),
      ORI(x31, x1, 511),

      ORI(x31, x1, -1),
      ORI(x31, x1, -3),
      ORI(x31, x1, -9),
      ORI(x31, x1, -98),
      ORI(x31, x1, -231),
      ORI(x31, x1, -510),

      ORI(x31, x11, 1),
      ORI(x31, x11, 2),
      ORI(x31, x11, 3),
      ORI(x31, x11, 7),
      ORI(x31, x11, 14),
      ORI(x31, x11, 28),
      ORI(x31, x11, 56),
      ORI(x31, x11, 133),
      ORI(x31, x11, 258),
      ORI(x31, x11, 511),

      ORI(x31, x11, -1),
      ORI(x31, x11, -3),
      ORI(x31, x11, -9),
      ORI(x31, x11, -98),
      ORI(x31, x11, -231),
      ORI(x31, x11, -510),

      ORI(x31, x16, 1),
      ORI(x31, x16, 2),
      ORI(x31, x16, 3),
      ORI(x31, x16, 7),
      ORI(x31, x16, 14),
      ORI(x31, x16, 28),
      ORI(x31, x16, 56),
      ORI(x31, x16, 133),
      ORI(x31, x16, 258),
      ORI(x31, x16, 511),

      ORI(x31, x16, -1),
      ORI(x31, x16, -3),
      ORI(x31, x16, -9),
      ORI(x31, x16, -98),
      ORI(x31, x16, -231),
      ORI(x31, x16, -510),

      DONE
    )

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
}
