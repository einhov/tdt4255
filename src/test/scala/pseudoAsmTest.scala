package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._

import implicitImm._

class pseudoAsmTest extends FlatSpec with Matchers {

  val initRegs = Map(0 -> Uint(0))
  val initMem = Map[Addr, Word]()
  val initMachine = MachineState.applyWithSetup(initMem, initRegs)

  it should "square numbers" in {
    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVasm.toRealOps(fileUtils.run("programs/square.s").unsafeRunSync()), initMachine, 1000, c, true).run
    } should be (true)
  }

  it should "run a naive recursive fibonnacci calculator" in {
    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVasm.toRealOps(fileUtils.run("programs/naiveFib.s").unsafeRunSync()), initMachine, 1000, c, true).run
    } should be (true)
  }

  it should "run a memoized fibonnacci calculator" in {
    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new TestRunner(RISCVasm.toRealOps(fileUtils.run("programs/memoFib.s").unsafeRunSync()), initMachine, 1000, c, true).run
    } should be (true)
  }
}
