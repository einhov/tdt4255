package Ov1

import chisel3._
import chisel3.core.Input

class IFBarrierSpy extends Bundle {
  val PC = UInt(32.W)
  val insn = new Instruction
}

class CPU extends Module {

  val io = IO(
    new Bundle {
      val setupSignals = Input(new SetupSignals)
      val testReadouts = Output(new TestReadouts)
      val regUpdates   = Output(new RegisterUpdates)
      val memUpdates   = Output(new MemUpdates)
      val currentPC    = Output(UInt(32.W))
      val currentInsn  = Output(new Instruction)
      val running      = Input(Bool())

      val IFBarrierSpy = Output(new IFBarrierSpy)
    }
  )

  /**
    You need to create the classes for these yourself
    */
  val IFBarrier  = Module(new IFBarrier).io
  // val IDBarrier  = Module(new IDBarrier).io
  // val EXBarrier  = Module(new EXBarrier).io
  // val MEMBarrier = Module(new MEMBarrier).io

  val IF  = Module(new InstructionFetch).io
  val ID  = Module(new InstructionDecode).io
  // val EX  = Module(new Execute).io
  val MEM = Module(new MemoryFetch).io

  /**
    setup stuff
    */
  IF.IMEMsetup     := io.setupSignals.IMEMsignals
  ID.registerSetup := io.setupSignals.registerSignals
  MEM.DMEMsetup    := io.setupSignals.DMEMsignals

  io.testReadouts.registerRead := ID.registerPeek
  io.testReadouts.DMEMread     := MEM.DMEMpeek

  /**
    spying stuff
    */
  io.regUpdates := ID.testUpdates
  io.memUpdates := MEM.testUpdates
  io.currentPC  := IF.PC
  io.currentInsn  := IF.insn

  io.IFBarrierSpy.PC := IFBarrier.PC
  io.IFBarrierSpy.insn := IFBarrier.insn

  /**
    Your signals here
   */
  IFBarrier.PC_in := IF.PC
  IFBarrier.insn_in := IF.insn

  // Make it compile
  IF.target := 0.U
  IF.branch := false.B
  IF.running := io.running
}
