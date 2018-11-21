package Ov1
import spire.math.{UInt => Uint}
import utilz._
import regNames._

object RISCVutils {

  import RISCVOPS._
  import riscala._

  type Reg               = Int
  type Imm               = Int
  type Addr              = Uint
  type Word              = Uint
  type RegUpdate         = (Reg, Word)
  type MemUpdate         = (Addr, Word)
  type RuntimeError      = String
  type MachineStateLog   = List[MachineState]

  case class PCTrace(t: List[Addr])
  case class PCLog(l: List[Addr])

  case class ExecutionLog(executionLog: MachineStateLog, opLog: List[OP], updateLog: List[StateUpdate], termination: Either[String, (String, Addr)]){
    def getDescriptiveLog: String = {
      (executionLog zip executionLog.drop(1) zip opLog)
        .map(x => Function.tupled(describeOp(x._2))(x._1))
        .zip(opLog.map(renderInstruction))
        .map(x => x._2 ++ "\n" ++ x._1)
        .mkString("--\n","\n\n","--\n") ++
        (termination match {
           case Right(x) => s"Final address was ${asHex(x._2.toInt)}"
           case Left(s) => s
         })
    }

    def getUpdateLog: (List[RegUpdate], List[MemUpdate], PCLog) = {
      val regUpdates = updateLog.map(_.r).flatten
      val memUpdates = updateLog.map(_.m).flatten
      val pcUpdates = PCLog(updateLog.map(_.pc))
      (regUpdates, memUpdates, pcUpdates)
    }

    def getInitState: MachineState = executionLog.head
  }


  case class RISCVProgram(instructions: Map[Addr, OP]){
    def getInstruction(addr: Addr): Either[RuntimeError, OP] =
      instructions.lift(addr).toRight(s"Attempted illegal read from address ${asHex(addr.toInt)}")

    def execute(timeOut: Int, m: MachineState): ExecutionLog = {
      val (stateLog, opLog, updateLog, error) = stepInstructions(m, this, timeOut)
      ExecutionLog(stateLog, opLog, updateLog, error.toLeft(("Program terminated successfully", stateLog.takeRight(2).head.pc)))
    }
  }
  object RISCVProgram {

    def apply(ops: List[RISCVasm.asmOP]): RISCVProgram = {
      RISCVasm.toRealOps(ops)
    }
  }


  case class MachineState(mem: Map[Uint, Uint], regs: Map[Int, Uint], pc: Uint){

    def updateMem(addr: Addr, word: Word): Option[MachineState] =
      mem.lift(addr).map{ _ =>
        copy(mem = mem.updated(addr, word), pc = pc + Uint(4))
      }

    def updateRegs(rd: Reg, word: Word): MachineState = {
      if(rd != 0)
        copy(regs = regs.updated(rd, word), pc = pc + Uint(4))
      else
        copy(pc = pc + Uint(4))
    }

    def readMem(addr: Addr): Option[Word] =
      mem.lift(addr)

  }
  object MachineState {

    def applyWithSetup(mem: Map[Uint, Uint], regs: Map[Int, Uint]): MachineState = {

      val regz = (0 to 31).foldLeft(regs){ case(regMap, idx) =>
        if (regMap.keys.exists(idx == _))
          regMap
        else
          regMap.updated(idx, Uint(0))
      }.updated(ra, Uint(1000))
        .updated(sp, Uint(1024))

      MachineState(mem, regz, Uint(0))
    }

    def apply(mem: Map[Uint, Uint], regs: Map[Int, Uint]): MachineState = {

      val regz = (0 to 31).foldLeft(regs){ case(regMap, idx) =>
        if (regMap.keys.exists(idx == _))
          regMap
        else
          regMap.updated(idx, Uint(0))
      }

      MachineState(mem, regz, Uint(0))
    }

    def randomizedRegs: MachineState = {
      val regs = List.range(1, 31).map((_, Uint(scala.util.Random.nextInt(1000)))).toMap
      val mem = Map[Uint,Uint]()
      MachineState(mem, regs, Uint(0))
    }
  }
}
