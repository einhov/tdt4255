package Ov1

import chisel3._
import chisel3.core.Input

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

      val IFBarrierSpy  = Output(new Ov1.IFBarrier.Contents)
      val IDBarrierSpy  = Output(new Ov1.IDBarrier.Contents)
      val EXBarrierSpy  = Output(new Ov1.EXBarrier.Contents)
      val MEMBarrierSpy = Output(new Ov1.MEMBarrier.Contents)
    }
  )

  /**
    You need to create the classes for these yourself
    */
  val IFBarrier  = Module(new IFBarrier).io
  val IDBarrier  = Module(new IDBarrier).io
  val EXBarrier  = Module(new EXBarrier).io
  val MEMBarrier = Module(new MEMBarrier).io

  val IF  = Module(new InstructionFetch).io
  val ID  = Module(new InstructionDecode).io
  val EX  = Module(new Execute).io
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
  io.currentPC  := IF.out.PC
  io.currentInsn  := IF.out.insn

  /**
    Your signals here
   */
  IFBarrier.in <> IF.out
  IDBarrier.in <> ID.out
  EXBarrier.in <> EX.out
  MEMBarrier.in <> MEM.out

  ID.in <> IFBarrier.out
  EX.in <> IDBarrier.out
  MEM.in <> EXBarrier.out
  ID.wb <> MEMBarrier.out

  io.IFBarrierSpy <> IFBarrier.out
  io.IDBarrierSpy <> IDBarrier.out
  io.EXBarrierSpy <> EXBarrier.out
  io.MEMBarrierSpy <> MEMBarrier.out

  // Branching
  IF.branch := EXBarrier.out.branch
  IF.target := EXBarrier.out.target
  IF.running := io.running
}
