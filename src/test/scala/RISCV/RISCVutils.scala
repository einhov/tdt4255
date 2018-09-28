package Ov1
import spire.math.{UInt => Uint}


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

  case class ExecutionLog(executionLog: MachineStateLog, opLog: List[OP], termination: Either[String, (String, Addr)]){
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

    def getUpdateLog: (List[RegUpdate], List[MemUpdate]) =
      collectExpectedUpdates(executionLog)

    def getInitState: MachineState = executionLog.head

  }

  case class RISCVProgram(instructions: Map[Addr, OP]){
    def getInstruction(addr: Addr): Either[RuntimeError, OP] =
      instructions.lift(addr).toRight(s"Attempted illegal read from address ${asHex(addr.toInt)}")

    def execute(timeOut: Int, m: MachineState): ExecutionLog = {
      val (stateLog, opLog, error) = stepInstructions(m, this, timeOut)
      ExecutionLog(stateLog, opLog, error.toLeft(("Program terminated successfully", stateLog.takeRight(2).head.pc)))
    }

  }

  object RISCVProgram {

    def apply(ops: List[RISCVasm.asmOP]): RISCVProgram = RISCVProgram(
      RISCVasm.toRealOpsWithNOP(ops ::: List(RISCVasm.DONE)).zipWithIndex.map {
        case(op, idx) =>
          (Uint(idx*4), op)
      }.toMap)
  }


  case class MachineState(mem: Map[Uint, Uint], regs: Map[Int, Uint], pc: Uint){
    def updateMem(addr: Addr, word: Word): Option[MachineState] =
      mem.lift(addr).map{ _ =>
        copy(mem = mem.updated(addr, word), pc = pc + Uint(4))
      }

    def updateRegs(rd: Reg, word: Word): MachineState =
      copy(regs = regs.updated(rd, word), pc = pc + Uint(4))

    def readMem(addr: Addr): Option[Word] =
      mem.lift(addr)
  }
  object MachineState {
    def apply(mem: Map[Uint, Uint], regs: Map[Int, Uint]): MachineState =
      MachineState(mem, regs, Uint(0))

    def randomizedRegs: MachineState = {
      val regs = List.range(1, 31).map((_, Uint(scala.util.Random.nextInt(1000)))).toMap
      val mem = Map[Uint,Uint]()
      MachineState(mem, regs, Uint(0))
    }
  }



  def hs(u: Uint): String = {
    s"0x${u.toInt.toHexString.toUpperCase()}"
  }

  def asNdigitBinary (source: Int, digits: Int): String = {
    val l  = source.toBinaryString
    val padLen = digits - l.size
    val pad = ("" /: (0 until padLen).map(_ => "0"))(_+_)
    pad + l
  }
  def as32BinarySpaced (source: Int): String = {
    val l  = source.toBinaryString
    val padLen = 32 - l.size
    val pad = ("" /: (0 until padLen).map(_ => "0"))(_+_)
    val s = pad + l
    s.take(8) + " " + s.drop(8).take(8) + " " + s.drop(16).take(8) + " " + s.drop(24).take(8)
  }

  def asHex(b: Int): String = f"0x$b%08x"
  def asBin(b: Int): String = {
    "0b" + asNdigitBinary(b, 32)
  }

  val lnOf2 = scala.math.log(2) // natural log of 2
  def log2(x: Int): Int = {
    (scala.math.log(x) / lnOf2).toInt
  }

  implicit class MapExt[K,V](val self: Map[K, V]) extends AnyVal {
    // gets a map value for specified key casting as required type
    def alter(k: K, v: V): Map[K,V] = {
      if(self.contains(k))
        self.updated(k, v)
      else
        self + (k -> v)
    }
  }
}
