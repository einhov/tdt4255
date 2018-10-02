package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import regNames._

class SquareTest extends FlatSpec with Matchers {

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


  it should "perform multiple calls to a function to square a number" in {

    val program = List(

      LABEL("setup"),
      LI(x8,  16), // desugars to ADDI(x8, x0, 16)
      LI(x10, 2),

      LABEL("loop1"),
      ADD(x2, x10, x0),
      JAL(x1, "square_number"),
      ADD(x10, x2, x0),
      BNE(x10, x8, "loop1"),

      DONE,

      // calling convention: x1 as return address, x2 - 4 as args
      // calling convention: x2 as return value
      LABEL("square_number"),

      // partial result
      ADD(x5, x0, x0),

      // loop counter
      ADD(x6, x0, x0),

      LABEL("loop2"),
      ADD(x5, x5, x2),
      ADDI(x6, x6, 1),
      BNE(x6, x2, "loop2"),

      // return jump
      ADD(x2, x0, x5),
      JR(x1) // desugars to JALR(x0, x1, 0)

    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, timeout, c, true).run
    } should be (true)
  }
}
