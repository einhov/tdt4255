package Ov1
import atto._, Atto._
import cats.implicits._

import spire.math.{UInt => Uint}
import spire.syntax.literals._

import org.scalatest.{Matchers, FlatSpec}
import testUtils._
import riscala._

class RISCVemuSpec extends FlatSpec with Matchers {
  it should "do thing" in {

    val myInstructions = makeArithmeticBlock(2, List(2, 3, 4), 2) ++
      makeArithmeticBlock(1, List(2, 3, 4), 2)

    val myProgram = assemble(myInstructions)
    val myInitReg: Map[Int, Uint] = Map(
      0  -> Uint(0x0),
      1  -> Uint(0xFFFFFFC0),
      2  -> Uint(0xFFFFFFFA),
      3  -> Uint(0x4),
      4  -> Uint(0x3),
      5  -> Uint(0xC0),
      6  -> Uint(0x10),
      7  -> Uint(0x20),
      8  -> Uint(0x40),
      9  -> Uint(0x80),
      10 -> Uint(0xC0),
      11 -> Uint(0x100),
      12 -> Uint(0x200),
      13 -> Uint(0x400),
      14 -> Uint(0x800),
      15 -> Uint(0xc00),
      16 -> Uint(0x1000),
      17 -> Uint(0x2000),
      18 -> Uint(0x4000),
      19 -> Uint(0x8000),
      20 -> Uint(0xC000),
      21 -> Uint(0x10000),
      22 -> Uint(0x20000),
      23 -> Uint(0x40000),
      24 -> Uint(0x80000),
      25 -> Uint(0xC0000),
      26 -> Uint(0x100000),
      27 -> Uint(0x200000),
      28 -> Uint(0x400000),
      29 -> Uint(0x800000),
      30 -> Uint(0xC00000),
      31 -> Uint(0x123))

    val myInitMem: Map[Uint, Uint] = Map()


    // println(printProgram(myInstructions))
    // println(printRegs(myInitReg))

    val what = runProgram(myInitReg, myInitMem, myProgram, 100, false)

    println(what.map(x => x._1))

    val huh = Uint(0xFFFFFFFE)
    println(s"0x${huh.toInt.toHexString.toUpperCase()}")

    true
  }
}
