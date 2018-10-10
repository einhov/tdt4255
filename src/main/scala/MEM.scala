package Ov1
import chisel3._
import chisel3.util._


class MemoryFetch() extends Module {
  val io = IO(
    new Bundle {


      // setup/test
      val DMEMsetup      = Input(new DMEMsetupSignals)
      val DMEMpeek       = Output(UInt(32.W))

      val testUpdates    = Output(new MemUpdates)
    })

  val DMEM = Module(new DMEM).io

  // setup
  DMEM.setup := io.DMEMsetup
  io.DMEMpeek := DMEM.dataOut
  io.testUpdates := DMEM.testUpdates

  // Make it compile
  DMEM := DontCare
}
