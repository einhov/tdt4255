package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

import RISCVutils._
import RISCVasm._
import assembler._
import regNames._

class MEM extends FlatSpec with Matchers {

  it should "read memory" in {

    // yeah, labels and shit
    val program = List(
      ADDI(x2, x2, 2),
      ADDI(x2, x2, 2),
      LW(x2, x0, 4),
      ADDI(x2, x2, 2),
      SW(x2, x0, 4),
      LW(x3, x0, 4),
      DONE
    )

    val initRegs = Map(
      x0 -> Uint(0),
      x1 -> Uint(1),
      x2 -> Uint(2),
      x3 -> Uint(3),
      x4 -> Uint(4),
      x5 -> Uint(0xFF),
      x6 -> Uint(0xFFFF),
      x7 -> Uint(123456),
      x8 -> Uint(0),
    )

    val initMem = Map(
      0 -> Uint(42),
      4 -> Uint(16)
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
        for((reg, value) <- initRegs) {
          poke(dut.io.setup, 1)
          poke(dut.io.running, 0)
          poke(dut.io.checkResult, 0)
          poke(dut.io.regsWriteEnable, 1)
          poke(dut.io.regsWriteData, BigInt(value.toInt))
          poke(dut.io.regsAddress, reg)
          step(1)
        }

        // Init memory
        for((addr, value) <- initMem) {
          poke(dut.io.setup, 1)
          poke(dut.io.running, 0)
          poke(dut.io.checkResult, 0)
          poke(dut.io.DMEMWriteEnable, 1)
          poke(dut.io.DMEMWriteData, value.toInt)
          poke(dut.io.DMEMAddress, addr.toInt)
          step(1)
        }

        poke(dut.io.setup, 0)
        poke(dut.io.running, 1)

        var cycle = 0;
        //step(4); cycle += 4 // Data is only available in the MEM barrier after four cycles
        for( _ <- 0 until instructions.length) {
          print(s"Cycle $cycle\n")
          val pc = peek(dut.io.MEMBarrierSpy.PC)
          val pc_scaled = (pc / 20).toInt
          val padding = (pc % 20) != 0

          {
            val data = peek(dut.io.EXBarrierSpy.data)
            val write_data = peek(dut.io.EXBarrierSpy.write_data)
            val rd = peek(dut.io.EXBarrierSpy.rd)
            val wb = peek(dut.io.EXBarrierSpy.wb)
            val addr = peek(dut.io.EXBarrierSpy.address)
            val read = peek(dut.io.EXBarrierSpy.read)
            val write = peek(dut.io.EXBarrierSpy.write)
            if(write === 1)
              print(s"EX Write:  rd: $rd, wb: $wb, addr: $addr, write: $write_data\n")
            else if(read === 1)
              print(s"EX Read:   rd: $rd, wb: $wb, addr: $addr\n")
            else
              print(s"EX Pass:   rd: $rd, wb: $wb, data: $data, addr: $addr, write: $write_data\n")
          }

          {
            val register_data = peek(dut.io.MEMBarrierSpy.register_data)
            val memory_data = peek(dut.io.MEMBarrierSpy.memory_data)
            val rd = peek(dut.io.MEMBarrierSpy.rd)
            val wb = peek(dut.io.MEMBarrierSpy.wb)
            val is_mem = peek(dut.io.MEMBarrierSpy.dataIsFromMem)
            print(s"MEM: rd: $rd, wb: $wb, isMem: $is_mem, rdata: $register_data, mdata: $memory_data\n")
          }
          step(1); cycle += 1
        }
      }
    } should be (true)
  }
}
