package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._

class BranchTests extends FlatSpec with Matchers {

  it should "handle a simple BNE looping program" in {

    // yeah, labels and shit
    val program = List(
      ADD(1, 1, 2),
      ADD(1, 1, 2),
      BNE(1, 3, "loop"),
      DONE
    )

    val initRegs = Map(
      0 -> Uint(0),
      1 -> Uint(0),
      2 -> Uint(1),
      3 -> Uint(4),
    )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, 100, c, true).run
    } should be (true)
  }


  it should "handle a simple BEQ looping program" in {

    // yeah, labels and shit
    val program = List(
      ADD(1, 1, 2),
      ADD(1, 1, 2),
      BEQ(1, 3, "loop"),
      DONE
    )

    val initRegs = Map(
      0 -> Uint(0),
      1 -> Uint(0),
      2 -> Uint(1),
      3 -> Uint(4),
      )

    val initMem = Map[Addr, Word]()
    val initMachine = MachineState(initMem, initRegs)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVProgram(program), initMachine, 100, c, true).run
    } should be (true)
  }
}
