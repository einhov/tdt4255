package Ov1
import chisel3._


/**
  While it would be convenient to just reuse the modules interface for
  setup this would lead to multiple drivers.
  */
class DMEM extends Module {
  val io = IO(
    new Bundle {
      val setup = Input(new DMEMsetupSignals)

      val writeEnable = Input(Bool())
      val dataIn      = Input(UInt(32.W))
      val dataAddress = Input(UInt(8.W))

      val dataOut     = Output(UInt(32.W))

      val testUpdates = Output(new MemUpdates)
    })

  val data = SyncReadMem(256, UInt(32.W))

  val addressSource = Wire(UInt(32.W))
  val dataSource = Wire(UInt(32.W))
  val writeEnableSource = Wire(Bool())

  // For loading data
  when(io.setup.setup){
    addressSource     := io.setup.dataAddress
    dataSource        := io.setup.dataIn
    writeEnableSource := io.setup.writeEnable
  }.otherwise {
    addressSource     := io.dataAddress
    dataSource        := io.dataIn
    writeEnableSource := io.writeEnable
  }

  io.testUpdates.writeEnable := writeEnableSource
  io.testUpdates.writeData := dataSource
  io.testUpdates.writeAddress := addressSource

  // Your code here
  io.dataOut := data(addressSource)
  when(writeEnableSource){
    data(addressSource) := dataSource
  }
}
