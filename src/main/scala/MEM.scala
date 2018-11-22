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

  DMEM.dataIn := io.in.write_data
  DMEM.dataAddress := io.in.address
  DMEM.writeEnable := io.in.write
  val readData = DMEM.dataOut

  io.out.rd := io.in.rd
  io.out.wb := io.in.wb
  io.out.register_data := io.in.data
  io.out.memory_data := readData
  io.out.dataIsFromMem := io.in.read

  io.out.PC := io.in.PC

  io.out.forward.operand := Mux(io.in.wb, io.in.rd, 0.U)
  io.out.forward.value := Mux(io.in.read, readData, io.in.data)
}
