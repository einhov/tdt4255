package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVOPS._

class SimpleLoop extends FlatSpec with Matchers {

  it should "run a simple looping program" in {

    val program = List(
      ADD(1, 1, 2),
      ADD(1, 1, 2),
      BNE(1, 3, -8),
      DONE
    )

    val initRegs = Map(
      1 -> Uint(0),
      2 -> Uint(1),
      3 -> Uint(4)
    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, 100, c, true).run
    } should be (true)
  }
}
