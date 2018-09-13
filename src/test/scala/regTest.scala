package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import testUtils._


class RegsSpec extends FlatSpec with Matchers {

  it should "be able to write and read instructions when in setup mode"

  def setupWrite(readAddr: BigInt, writeAddr: BigInt, data: BigInt): Map[String, BigInt] = Map(
    "setup" -> 1,
    "writeEnable" -> 1,
    "dataIn" -> data
  )

  val ins = (0 to 32).map(ii =>
    CycleTask[Registers](
      ii,
      // d => d.poke(d.dut.io.setup, 1)
    ))

}
