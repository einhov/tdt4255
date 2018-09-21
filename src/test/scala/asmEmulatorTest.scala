package Ov1
import Ov1.RISCVOPS._
import atto._, Atto._
import cats.implicits._

import spire.math.{UInt => Uint}
import spire.syntax.literals._

import org.scalatest.{Matchers, FlatSpec}
import riscala._

import RISCVOPS._
import RISCVutils._

class RISCVemuSpec extends FlatSpec with Matchers {

  it should "do thing" in {

    val myInstructions =
      makeArithmeticBlock(2, List(2, 3, 4), 2) ++
      makeArithmeticBlock(1, List(2, 3, 4), 2)

    val log = RISCVProgram(myInstructions).execute(100, MachineState.randomizedRegs)

    println(log.getDescriptiveLog)
    println(log.getUpdateLog._1.map{ case(x,y) => s"r$x <- ${hs(y)}"}.mkString("\n"))


  }
}
