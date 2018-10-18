package Ov1
import chisel3._
import chisel3.util._


class MemoryFetch() extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new EXBarrier.Contents)
      val out = Output(new MEMBarrier.Contents)

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

  DMEM.dataIn := io.in.data
  DMEM.dataAddress := io.in.address
  DMEM.writeEnable := io.in.write
  val readData = DMEM.dataOut

  io.out.wb := io.in.wb
  io.out.data := Mux(io.in.read, readData, io.in.data)
  io.out.rd := io.in.rd
}
