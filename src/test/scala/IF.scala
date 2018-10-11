package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import assembler._

class IF extends FlatSpec with Matchers {

  it should "fetch instructions" in {

    // yeah, labels and shit
    val program = List(
      LABEL("loop1"),
      ADD(1, 1, 2),
      BNE(1, 2, "loop1"),
      JR(1),
      DONE
    )

    val opcodes = Array(
      51, 99, 103, 0, 0
    );

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      new PeekPokeTester(c) {
        val instructions = assembleProgram(RISCVProgram(program))
        for( ii <- 0 until instructions.length) {
          val insn = instructions(ii).toInt
          poke(dut.io.setup, 1)
          poke(dut.io.running, 0)
          poke(dut.io.checkResult, 0)
          poke(dut.io.IMEMAddress, ii*4)
          poke(dut.io.IMEMWriteData, instructions(ii).toInt)
          step(1)
        }

        poke(dut.io.setup, 0)
        poke(dut.io.running, 1)

        var cycle = 0;
        step(1); cycle += 1 // Data is only available in the IF barrier after one cycle
        for( _ <- 0 until instructions.length) {
          val pc = peek(dut.io.IFBarrierSpy.PC)
          val opcode = peek(dut.io.IFBarrierSpy.insn.instruction) & 127
          val expected = if((pc % 20) == 0) opcodes((pc / 20).toInt) else 0
          expect(opcode == expected, s"Expected $expected, got $opcode at cycle $cycle")
          step(1); cycle += 1
        }
      }
    } should be (true)
  }
}
