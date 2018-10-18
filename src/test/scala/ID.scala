package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import assembler._

class ID extends FlatSpec with Matchers {

  it should "decode instructions" in {

    // yeah, labels and shit
    val program = List(
      LABEL("loop1"),
      ADD(1, 1, 14),
      BNE(2, 12, "loop1"),
      JALR(0, 10, 42),         // Positive I-Type
      JALR(0, 10, -42),        // Negative I-Type
      JAL(0, "jal_target"),    // Positive J-Type
      JAL(0, "loop1"),         // Negative J-Type
      LABEL("jal_target"),
      SW(0, 1, 42),            // Positive S-Type
      SW(0, 2,-42),            // Negative S-Type
      BEQ(1, 2, "beq_target"), // Positive B-Type
      BEQ(1, 2, "loop1"),      // Negative B-Type
      LABEL("beq_target"),
      LUI(5, 42),              // Positive U-Type
      SRLI(0, 1, 14),          // Positive SHMT-Type
      DONE
    )

    val regWrites = Array(
      1,    // ADD
      0,    // BNE
      1, 1, // JALR
      1, 1, // JAL
      0, 0, // SW
      0, 0, // BEQ
      1,    // LUI
      1,    // SRLI
      0, 0  // NOP
    )
    val jumps = Array(
      0,
      0,
      1, 1,
      1, 1,
      0, 0,
      0, 0,
      0,
      0,
      0, 0
    )
    val ops = Array(
      0,
      15,
      15, 15,
      15, 15,
      0, 0,
      15, 15,
      15,
      8,
      15, 15
    )
    val op1s = Array(
      1,
      2,
      10, 10,
      0, 0,
      1, 2,
      1, 1,
      5,
      1,
      0, 0
    )
    val op2s = Array(
      14,
      12,
      0, 0,
      0, 0,
      0, 0,
      2, 2,
      0,
      0,
      0, 0
    )
    val imms = Array(
      14,
      -20,
      42, -42,
      40, -100,
      42, -42,
      40, -180,
      42 << 12,
      14,
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
        step(2); cycle += 2 // Data is only available in the ID barrier after two cycles
        for( _ <- 0 until instructions.length) {
          val pc = peek(dut.io.IDBarrierSpy.PC)
          val pc_scaled = (pc / 20).toInt
          val padding = (pc % 20) != 0

          val regWrite = peek(dut.io.IDBarrierSpy.controlSignals.regWrite)
          val eregWrite = if(!padding) regWrites(pc_scaled) else 0
          val jump = peek(dut.io.IDBarrierSpy.controlSignals.jump)
          val ejump = if(!padding) jumps(pc_scaled) else 0
          val alu = peek(dut.io.IDBarrierSpy.ALUop)
          val ealu = if(!padding) ops(pc_scaled) else 15
          val op1 = peek(dut.io.IDBarrierSpy.rs1)
          val eop1 = if(!padding) op1s(pc_scaled) else 0
          val op2 = peek(dut.io.IDBarrierSpy.rs2)
          val eop2 = if(!padding) op2s(pc_scaled) else 0
          val imm = peek(dut.io.IDBarrierSpy.immediate).toInt
          val eimm = if(!padding) imms(pc_scaled) else 0

          expect(regWrite == eregWrite, s"regWrite Expected $eregWrite, got $regWrite at cycle $cycle, pc $pc")
          expect(jump == ejump, s"jump Expected $ejump, got $jump at cycle $cycle, pc $pc")
          expect(alu == ealu, s"alu Expected $ealu, got $alu at cycle $cycle, pc $pc")
          expect(op1 == eop1, s"op1 Expected $eop1, got $op1 at cycle $cycle, pc $pc")
          expect(op2 == eop2, s"op2 Expected $eop2, got $op2 at cycle $cycle, pc $pc")
          expect(imm == eimm, s"imm Expected $eimm, got $imm at cycle $cycle, pc $pc")
          step(1); cycle += 1
        }
      }
    } should be (true)
  }
}
