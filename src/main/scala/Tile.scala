package Ov1

import chisel3._
import chisel3.util._
import chisel3.core.Input
import chisel3.iotesters.PeekPokeTester


class Tile() extends Module{

  val io = IO(
    new Bundle {
      val DMEMWriteData          = Input(UInt(32.W))
      val DMEMAddress            = Input(UInt(32.W))
      val DMEMWriteEnable        = Input(Bool())
      val DMEMReadData           = Output(UInt(32.W))

      val regsWriteData          = Input(UInt(32.W))
      val regsAddress            = Input(UInt(5.W))
      val regsWriteEnable        = Input(Bool())
      val regsReadData           = Output(UInt(32.W))

      val regsDeviceWriteEnable  = Output(Bool())
      val regsDeviceWriteData    = Output(UInt(32.W))
      val regsDeviceWriteAddress = Output(UInt(5.W))

      val memDeviceWriteEnable   = Output(Bool())
      val memDeviceWriteData     = Output(UInt(32.W))
      val memDeviceWriteAddress  = Output(UInt(32.W))

      val IMEMWriteData          = Input(UInt(32.W))
      val IMEMAddress            = Input(UInt(32.W))

      val setup                  = Input(Bool())
      val running                = Input(Bool())
      val checkResult            = Input(Bool())

      val currentPC              = Output(UInt())

      val IFBarrierSpy           = Output(new IFBarrier.Contents)
      val IDBarrierSpy           = Output(new IDBarrier.Contents)
      val EXBarrierSpy           = Output(new EXBarrier.Contents)
      val MEMBarrierSpy          = Output(new MEMBarrier.Contents)
  
      val freeze = Output(Bool())
    })

  val CPU = Module(new CPU).io
  CPU.setupSignals.IMEMsignals.address     := io.IMEMAddress
  CPU.setupSignals.IMEMsignals.instruction := io.IMEMWriteData
  CPU.setupSignals.IMEMsignals.setup       := io.setup

  CPU.setupSignals.DMEMsignals.writeEnable := io.DMEMWriteEnable
  CPU.setupSignals.DMEMsignals.dataAddress := io.DMEMAddress
  CPU.setupSignals.DMEMsignals.dataIn      := io.DMEMWriteData
  CPU.setupSignals.DMEMsignals.setup       := io.setup

  CPU.setupSignals.registerSignals.readAddress  := io.regsAddress
  CPU.setupSignals.registerSignals.writeEnable  := io.regsWriteEnable
  CPU.setupSignals.registerSignals.writeAddress := io.regsAddress
  CPU.setupSignals.registerSignals.writeData    := io.regsWriteData
  CPU.setupSignals.registerSignals.setup        := io.setup

  io.DMEMReadData := CPU.testReadouts.DMEMread
  io.regsReadData := CPU.testReadouts.registerRead

  io.regsDeviceWriteAddress := CPU.regUpdates.writeAddress
  io.regsDeviceWriteEnable  := CPU.regUpdates.writeEnable
  io.regsDeviceWriteData    := CPU.regUpdates.writeData

  io.memDeviceWriteAddress  := CPU.memUpdates.writeAddress
  io.memDeviceWriteEnable   := CPU.memUpdates.writeEnable
  io.memDeviceWriteData     := CPU.memUpdates.writeData

  io.currentPC := CPU.currentPC
  io.IFBarrierSpy <> CPU.IFBarrierSpy
  io.IDBarrierSpy <> CPU.IDBarrierSpy
  io.EXBarrierSpy <> CPU.EXBarrierSpy
  io.MEMBarrierSpy <> CPU.MEMBarrierSpy
  CPU.running := io.running
  io.freeze := CPU.freeze
}



