package Ov1
import chisel3._
import chisel3.util._


class WriteBack() extends Module {
  val io = IO(
    new Bundle {
      val in = Input(new MEMBarrier.Contents)
      
      val rd = Output(UInt(5.W))
      val wb = Output(Bool())
      val data = Output(UInt(32.W))
    })

  io.rd := io.in.rd
  io.wb := io.in.wb
  io.data := Mux(io.in.dataIsFromMem, io.in.memory_data, io.in.register_data)
}
