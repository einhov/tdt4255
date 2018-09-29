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


  def stepInstruction(m: MachineState, p: RISCVProgram): Either[RuntimeError, (OP, MachineState, StateUpdate)] =
    for {
      op                <- p.getInstruction(m.pc)
      (nextM, logEntry) <- applyOperation(op)(m)
    } yield (op, nextM, logEntry)


  def stepInstructions(init: MachineState, program: RISCVProgram, timeOut: Int): (MachineStateLog, List[OP], List[StateUpdate], Option[RuntimeError]) = {

    def stepHelper(m: MachineState, stepsLeft: Int): Either[RuntimeError, (OP, MachineState, StateUpdate)] =
      for {
        step <- Either.cond(stepsLeft > 0, (), s"timed out after $timeOut steps")
        next <- stepInstruction(m, program)
      } yield next


    def go(stateLog: MachineStateLog, opLog: List[OP], updateLog: List[StateUpdate], m: MachineState, stepsLeft: Int): (MachineStateLog, List[OP], List[StateUpdate], Option[RuntimeError]) = {
      if(m.pc === Uint(0xF01D1EF7))
        (stateLog, opLog, updateLog, None)
      else
        stepHelper(m, stepsLeft) match {
          case Left(e)     => (stateLog, opLog, updateLog, Some(e))
          case Right((op, machineState, update)) => go(machineState :: stateLog, op :: opLog, update :: updateLog, machineState,  stepsLeft - 1)
        }
    }

    val(stateLog, opLog, updateLog, error) = go(List(init), Nil, Nil, init, timeOut)
    (stateLog.reverse, opLog.reverse, updateLog.reverse, error)
  }
}
