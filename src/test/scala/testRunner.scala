package Ov1
import chisel3.iotesters._
import spire.math.{UInt => Uint}

import RISCVutils._
import assembler._
import spire.math.{UInt => Uint}

class TestRunner(program: RISCVProgram, init: MachineState, stepsTimeOut: Int, c: Tile, verbose: Boolean = false) {

  def checkMemUpdate(d: PeekPokeTester[Tile],
                     expected: List[(Addr, Word)]): List[(Addr, Word)] = {

    if(d.peek(d.dut.io.memDeviceWriteEnable) == 1){
      if(expected.isEmpty) {
        d.fail
        expected.tail // the joys of oop
      }
      else {
        d.expect(d.dut.io.memDeviceWriteAddress, BigInt(expected.head._1.toInt), "Wrong write address")
        d.expect(d.dut.io.memDeviceWriteAddress, BigInt(expected.head._1.toInt), "Wrong write value")
        expected.tail
      }
    }
    else
      expected
  }

  def checkRegUpdate(d: PeekPokeTester[Tile],
                     expected: List[(Reg, Word)]): List[(Reg, Word)] = {

    if(d.peek(d.dut.io.regsDeviceWriteEnable) == 1){
      if(expected.isEmpty) {
        d.fail
        expected// the joys of oop
      }
      else {
        d.expect(d.dut.io.regsDeviceWriteAddress, BigInt(expected.head._1.toInt), "Wrong register write address")
        d.expect(d.dut.io.regsDeviceWriteAddress, BigInt(expected.head._1.toInt), "Wrong register write value")
        expected.tail
      }
    }
    else
      expected
  }



  def stepOne(
    expectedRegUpdates: List[(Reg, Word)],
    expectedMemUpdates: List[(Addr, Word)],
    d: PeekPokeTester[Tile]
  ): (List[(Int,Uint)],List[(Uint,Uint)]) = {
    val nextRegs = checkRegUpdate(d, expectedRegUpdates)
    val nextMem  = checkMemUpdate(d, expectedMemUpdates)

    (nextRegs, nextMem)
  }



  def run = new PeekPokeTester(c){

    def setup(instructions: List[Uint], regs: List[(Reg, Word)], mem: List[(Addr, Word)], d: PeekPokeTester[Tile]) = {
      regs.foreach{ case(reg, word) =>
        d.poke(d.dut.io.setup, 1)
        d.poke(d.dut.io.running, 0)
        d.poke(d.dut.io.checkResult, 0)
        d.poke(d.dut.io.regsWriteEnable, 1)
        d.poke(d.dut.io.regsWriteData, BigInt(word.toInt))
        d.poke(d.dut.io.regsAddress, reg)
        step(1)
      }

      mem.foreach { case (addr, word) =>
        d.poke(d.dut.io.setup, 1)
        d.poke(d.dut.io.running, 0)
        d.poke(d.dut.io.checkResult, 0)
        d.poke(d.dut.io.DMEMWriteEnable, 1)
        d.poke(d.dut.io.DMEMWriteData, addr.toInt)
        d.poke(d.dut.io.DMEMAddress, word.toInt)
        step(1)
      }

      for( ii <- 0 until instructions.length * 5) {
        d.poke(d.dut.io.setup, 1)
        d.poke(d.dut.io.running, 0)
        d.poke(d.dut.io.checkResult, 0)
        d.poke(d.dut.io.IMEMAddress, ii*4)
        if( ii % 5 == 0 )
          d.poke(d.dut.io.IMEMWriteData, instructions(ii/5).toInt)
        else
          d.poke(d.dut.io.IMEMWriteData, 0)

        step(1)
      }

      d.poke(d.dut.io.setup, 0)
      d.poke(d.dut.io.running, 1)
    }

    def stepMany(
      timeOut: Int,
      expectedRegUpdates: List[(Reg, Word)],
      expectedMemUpdates: List[(Addr, Word)],
      d: PeekPokeTester[Tile]
    ): Unit = {

      if(timeOut == 0) {
        println("Looks like you're out of time")
        d.fail
      }
      else if(Uint(d.peek(d.dut.io.currentPC).toInt) == Uint(0xF07D1EF7)){
        if(expectedMemUpdates.isEmpty && expectedRegUpdates.isEmpty) {
          println("You're winner!")
        }
        else{
          println("Program terminated successfully, but expected reg/mem updates have not happened.")
          d.fail
        }
      }
      else {
        val (nextReg, nextMem) = stepOne(expectedRegUpdates, expectedMemUpdates, d)
        step(1)
        stepMany(timeOut - 1, nextReg, nextMem, d)
      }
    }


    val log = program.execute(stepsTimeOut, init)

    val maxSteps = (log.opLog.size + 3)*6

    val (regUpdates, memUpdates) = log.getUpdateLog
    val initReg = log.getInitState.regs
    val initMem = log.getInitState.mem
    val machineOps = assembleProgram(program)

    if(verbose){
      println("This test is called with the verbose flag set to true")
      println("Verbose output as follows\n")
      println(log.getDescriptiveLog)
      println("\nVerbose output done\n\n")
    }

    setup(machineOps, initReg.toList, initMem.toList, this)
    stepMany(maxSteps, regUpdates, memUpdates, this)
  }
}
