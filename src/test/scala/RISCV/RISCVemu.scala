package Ov1
import atto._, Atto._
import cats.implicits._

import org.scalatest.{Matchers, FlatSpec}

import scala.util.Random.shuffle

// Unfortunate name conflict with chisel cleared up
import spire.math.{UInt => Uint}

object riscala {
  import RISCVOPS._
  import assembler._
  import RISCVutils._


  def stepInstruction(m: MachineState, p: RISCVProgram): Either[RuntimeError, (OP, MachineState)] =
    for {
      op    <- p.getInstruction(m.pc)
      nextM <- applyOperation(op)(m)
    } yield (op, nextM)


  def stepInstructions(init: MachineState, program: RISCVProgram, timeOut: Int): (MachineStateLog, List[OP], Option[RuntimeError]) = {

    def stepHelper(m: MachineState, stepsLeft: Int): Either[RuntimeError, (OP, MachineState)] =
      for {
        step <- Either.cond(stepsLeft > 0, (), s"timed out after $timeOut steps")
        next <- stepInstruction(m, program)
      } yield next


    def go(stateLog: MachineStateLog, opLog: List[OP], m: MachineState, stepsLeft: Int): (MachineStateLog, List[OP], Option[RuntimeError]) = {
      if(m.pc === Uint(0xF01D1EF7))
        (stateLog, opLog, None)
      else
        stepHelper(m, stepsLeft) match {
          case Left(e)     => (stateLog, opLog, Some(e))
          case Right(next) => go(next._2 :: stateLog, next._1 :: opLog, next._2, stepsLeft - 1)
        }
    }

    val(stateLog, opLog, error) = go(List(init), Nil, init, timeOut)
    (stateLog.reverse, opLog.reverse, error)
  }


  // Could be changed to use ops to make it a little faster I suppose
  def collectExpectedUpdates(ms: MachineStateLog): (List[RegUpdate], List[MemUpdate], List[Addr]) = {
    def compareStates(old: MachineState, next: MachineState) = {
      val memUp = next.mem.toSet.diff(old.mem.toSet).toList.headOption
      val regUp = next.regs.toSet.diff(old.regs.toSet).toList.headOption
      (memUp, regUp)
    }

    val (memUps, regUps) = ms.zip(ms.drop(1)).map(Function.tupled(compareStates)).unzip
    val PCUps = ms.map(_.pc)
    (regUps.flatten, memUps.flatten, PCUps)
  }
}
