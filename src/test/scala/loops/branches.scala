package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

class BranchTests extends FlatSpec with Matchers {

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

  it should "handle a simple BNE looping program" in {

    val program = List(
      LABEL("loop"),
      ADD(x2, x2, x2),
      SLT(x1, x2, x5),
      BNE(x0, x1, "loop"),
      DONE
    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, 100, c, true).run
    } should be (true)
  }


  it should "handle a simple BEQ looping program" in {

    val program = List(
      LABEL("loop"),
      SLL(x3, x3, x2),
      SLTI(x4, x3, 0xFF),
      XORI(x5, x4, 0x1),
      BEQ(x0, x5, "loop"),
      DONE
    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, 100, c, true).run
    } should be (true)
  }


  it should "handle a looping program with BGE and BGEU" in {

    val program = List(
      LABEL("loop1"),
      ADDI(x8, x8, -5),
      BGEU(x12, x8, "loop1"),

      ADDI(x8, x8, -40),

      LABEL("loop2"),
      ADDI(x8, x8, -5),
      BGE(x8, x12, "loop2"),

      DONE
    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, 100, c, true).run
    } should be (true)
  }


  it should "handle a looping program with BLT and BLTU" in {

    val program = List(
      LABEL("loop1"),
      ADDI(x8, x8, -5),
      BLTU(x8, x12, "loop1"),

      ADDI(x8, x8, -40),

      LABEL("loop2"),
      ADDI(x8, x8, -5),
      BLT(x12, x8, "loop2"),

      DONE
    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, 100, c, true).run
    } should be (true)
  }

}
