package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import spire.math.{UInt => Uint}

class TestRunner extends FlatSpec with Matchers {

  def stepOne(
    // address, value
    expectedRegUpdates: List[(Uint, Uint)],
    expectedMemUpdates: List[(Uint, Uint)],
    d: PeekPokeTester[Tile]
  ): (List[(Uint,Uint)],List[(Uint,Uint)]) = {
    val deviceIO = d.peek(d.dut.io)

    val nextRegs =
      if((deviceIO("regsDeviceWriteEnable") == 1)){
        if(!(expectedRegUpdates.isEmpty)){
          d.expect(d.dut.io.regsDeviceWriteAddress, expectedRegUpdates.head._1.toBigInt)
          d.expect(d.dut.io.regsDeviceWriteData, expectedRegUpdates.head._2.toBigInt)
          expectedRegUpdates.tail
        }
        else{
          d.fail
          expectedRegUpdates
        }
      }
      else{
        expectedRegUpdates
      }

    val nextMem =
      if((deviceIO("memDeviceWriteEnable") == 1)){
        if(!(expectedMemUpdates.isEmpty)){
          d.expect(d.dut.io.memDeviceWriteAddress, expectedMemUpdates.head._1.toBigInt)
          d.expect(d.dut.io.memDeviceWriteData, expectedMemUpdates.head._2.toBigInt)
          expectedMemUpdates.tail
        }
        else{
          d.fail
          expectedMemUpdates
        }
      }
      else{
        expectedMemUpdates
      }

    (nextRegs, nextMem)
  }


  it should "run" in {

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>

      def run(c: Tile) = new PeekPokeTester(c){

        // might explode the stack lol
        def stepMany(
          timeOut: Int,
          expectedRegUpdates: List[(Uint, Uint)],
          expectedMemUpdates: List[(Uint, Uint)],
          d: PeekPokeTester[Tile]
        ): Unit = {

          if(timeOut == 0)
            d.fail
          else if(Uint(d.peek(d.dut.io.currentPC).toInt) == Uint(0xF07D1EF7)){
            if(expectedMemUpdates.isEmpty && expectedRegUpdates.isEmpty)
              ()
            else
              d.fail
          }
          else {
            val (nextReg, nextMem) = stepOne(expectedRegUpdates, expectedMemUpdates, d)
            step(1)
            stepMany(timeOut - 1, nextReg, nextMem, d)
          }
        }

        val magicTimeOut = 100
        val magicMem = List[(Uint,Uint)]()
        val magicReg = List[(Uint,Uint)]()

        stepMany(magicTimeOut, magicMem, magicReg, this)

      }

      run(c)

    } should be(true)

  }
}
