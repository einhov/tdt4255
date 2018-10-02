package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

class JumpTests extends FlatSpec with Matchers {

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

    x10 -> Uint(0xFFFFFFFF),
    x11 -> Uint(0xFFFFFFFF),
    x12 -> Uint(0xFFFFFFCE),
    x13 -> Uint(0xFFFFFFAA),
    x14 -> Uint(0xFFFFFE),
    x15 -> Uint(0x8FFFFFFE),
    x16 -> Uint(0x7FFFFFFE),
    x17 -> Uint(0x7FFFFFFF),
    x18 -> Uint(0x7FFFFFAA),
    )

  it should "handle a simple JAL jumpfest program" in {

    val program = List(
      ADD(x1, x0, x2),
      JAL(x7, "j3"),

      LABEL("j2"),
      ADD(x1, x0, x3),
      JAL(x7, "j5"),

      LABEL("j3"),
      SW(x1, x0, 10),
      JAL(x17, "j2"),

      LABEL("j4"),
      ADD(x1, x0, x5),
      JAL(x17, "j6"),

      LABEL("j5"),
      LW(x1, x0, 10),
      JAL(x17, "j4"),


      LABEL("j6"),
      DONE
    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }


  it should "handle JALR jumps" in {

    val program = List(

      ADDI(x2, x0, 10),
      JAL(x1, "j2"),
      JAL(x1, "done"),

      LABEL("j1"),
      ADD(x3, x3, x3),
      JALR(x0, x1, 0),

      LABEL("j2"),
      SW(x1, x0, 0),
      JAL(x1, "j1"),
      LW(x1, x0, 0),
      JALR(x0, x1, 0),

      LABEL("jump hell"),
      ADD(x1, x1, x1),
      JAL(x1, "jump hell"),

      LABEL("done"),
      DONE
    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
}
