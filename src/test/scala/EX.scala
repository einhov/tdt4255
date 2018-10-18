package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import assembler._

class EX extends FlatSpec with Matchers {

  it should "decode instructions" in {

    // yeah, labels and shit
    val program = List(
      ADD(2, 1, 14),
      LABEL("loop"),
      ADD(2, 2, 14),
      ADDI(2, 1, -255),
      AND(2, 0x15, 0x4),
      XOR(2, 0x15, 0x14),
//      JAL(0, "loop"),
      BGE(3, 1, "loop"),
      BEQ(1, 1, "loop"),
      DONE
    )

    val datas = Array(
      15, 16,
      -254,
      0x4,
      1,
      0, 0,
      0, 0
    )

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

        // Init registers
        for(reg <- 0 until 31) {
          poke(dut.io.setup, 1)
          poke(dut.io.running, 0)
          poke(dut.io.checkResult, 0)
          poke(dut.io.regsWriteEnable, 1)
          poke(dut.io.regsWriteData, BigInt(reg))
          poke(dut.io.regsAddress, BigInt(reg))
          step(1)
        }

        poke(dut.io.setup, 0)
        poke(dut.io.running, 1)

        var cycle = 0;
        step(3); cycle += 3 // Data is only available in the EX barrier after three cycles
        for( _ <- 0 until instructions.length * 2) {
          val pc = peek(dut.io.EXBarrierSpy.PC)
          val pc_scaled = (pc / 20).toInt
          val padding = (pc % 20) != 0

          val data = peek(dut.io.EXBarrierSpy.data).toInt
          val edata = if(!padding) datas(pc_scaled) else 0

          expect(data == edata, s"data Expected $edata, got $data at cycle $cycle, pc $pc")
          step(1); cycle += 1
        }
      }
    } should be (true)
  }
}
