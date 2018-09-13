package Ov1
import chisel3._

/**
  When setup is enabled data is written to the instruction memory.
  In normal operation this memory is write only (no self modifying code)
  */
class IMEM() extends Module {
  val io = IO(
    new Bundle {

      val setup = Input(new IMEMsetupSignals)

      val instructionAddress = Input(UInt(32.W))
      val instruction        = Output(UInt(32.W))

    })

  /**
    SyncReadMem will output the value of the address signal set in the previous cycle.
    */
  val instructions = SyncReadMem(256, UInt(32.W))

  // The address we want to read at during operation. During setup it acts as a write address
  // leading to the somewhat uninformative name shown here.
  val addressSource = Wire(UInt(32.W))

  when(io.setup.setup){
    addressSource := io.setup.address
  }.otherwise {
    addressSource := io.instructionAddress
  }

  // For loading data
  when(io.setup.setup){
    instructions(addressSource) := io.setup.instruction
  }

  io.instruction := instructions(addressSource)
}
