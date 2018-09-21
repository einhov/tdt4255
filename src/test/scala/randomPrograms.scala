package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}

import RISCVutils._


class RandomProgram extends FlatSpec with Matchers {

  it should s"run some random arithmetic instructions" in {
    val testProgram =
      makeArithmeticBlock(2, List(2, 3, 4), 2) ++
        makeArithmeticBlock(1, List(2, 3, 4), 2)

    val program = RISCVProgram(testProgram)

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(program, MachineState.randomizedRegs, 100, c).run
    } should be (true)
  }
}
