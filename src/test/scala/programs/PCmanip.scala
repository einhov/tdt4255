package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

class ProgramCounterTest extends FlatSpec with Matchers {

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


  it should "handle AUIPC and LUI" in {

    val program = List(

      NOP,
      AUIPC(x1, 100),
      LUI(x2, 100),
      ADDI(x2, x2, 0x14),
      ADDI(x3, x2, 0x4),

      BEQ(x1, x2, "NOP_PADDED"),
      BEQ(x1, x3, "NOT_NOP_PADDED"),

      ADDI(x1, x1, 125),
      J("gopher_hangout"),


      LABEL("NOP_PADDED"),
      ADDI(x1, x1, 124),
      DONE,

      LABEL("NOT_NOP_PADDED"),
      ADDI(x1, x1, 123),

      LABEL("gopher_hangout"),
      ADDI(x2, x2, 10),
      LABEL("lol no generics"),
      NOP

    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
}
