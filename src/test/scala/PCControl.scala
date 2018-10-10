package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}

class PCControl extends FlatSpec with Matchers {

  it should "increment PC" in {

    iotesters.Driver.execute(() => new InstructionFetch(), new TesterOptionsManager) { c =>
      new PeekPokeTester(c) {
        poke(dut.io.branch, false.B)
        for(n <- 0 until 100) {
          expect(dut.io.PC, (n * 4).U)
          step(1)
        }
      }
    } should be (true)
  }

  it should "branch" in {
    iotesters.Driver.execute(() => new InstructionFetch(), new TesterOptionsManager) { c =>
      new PeekPokeTester(c) {
        poke(dut.io.branch, true.B)
        poke(dut.io.target, 2000.U)
        step(1)

        poke(dut.io.branch, false.B)
        for(n <- 0 until 100) {
          expect(dut.io.PC, (n * 4 + 2000).U)
          step(1)
        }
      }
    } should be (true)
  }
}
