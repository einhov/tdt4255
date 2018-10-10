package Ov1
import chisel3._
import chisel3.util.{ BitPat, MuxCase }

/**
  */
class InstructionDecode extends Module {
  val io = IO(
    new Bundle {



      // setup/test
      val registerSetup     = Input(new RegisterSetupSignals)
      val registerPeek      = Output(UInt(32.W))

      val testUpdates       = Output(new RegisterUpdates)
    })

  val registers    = Module(new Registers).io
  val control      = Module(new Control).io

  // Wire up register setup
  registers.setup := io.registerSetup
  io.registerPeek := registers.readData1
  io.testUpdates  := registers.testUpdates

  // Make it compile
  io.registerPeek := 0.U
  io.testUpdates := DontCare

  registers := DontCare
  control := DontCare
}
