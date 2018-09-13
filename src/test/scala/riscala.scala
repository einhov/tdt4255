package Ov1
import atto._, Atto._
import cats.implicits._

import org.scalatest.{Matchers, FlatSpec}
import testUtils._

import scala.util.Random.shuffle

// Unfortunate name conflict with chisel cleared up
import spire.math.{UInt => Uint}

object riscala {
  import RISCVOPS._

  def runProgram(
    initRegs: Map[Int, Uint],
    initMem: Map[Uint, Uint],
    program: Map[Uint, OP],
    timeOut: Int,
    silent: Boolean = true): Either[(String,MachineState), (String,MachineState)] =
  {
    val initMachine = MachineState(initMem, initRegs, Uint(0))

    def step(m: MachineState): Either[String, (MachineState, String)] = {
      program.lift(m.pc).map{ op =>
        op.run(m)
      }.getOrElse(Left(s"Illegal fetch at PC = ${m.pc}"))
    }

    def stepMany(t: Int, m: MachineState, s: String): Either[(String, MachineState), (String, MachineState)] = {
      if(t == 0){
        Left((s ++ s"\nTimed out after $timeOut steps", m))
      }
      else if(m.pc == Uint(0xF01D1EF7))
      {
        Right((s, m))
      }
      else{
        val n = step(m)
        n match {
          case Left(s) => Left((s ++ "\n\n", m))
          case Right((m, s2)) => stepMany(t - 1, m, s ++ "\n\n" ++ s2)
        }
      }
    }

    stepMany(timeOut, initMachine, "")

  }


  def makeArithmeticOp(result: Reg, inputs: List[Reg]): OP = {
    val in1 = shuffle(inputs).head
    val in2 = shuffle(inputs).head

    val ops = List(
      ADD(result, in1, in2),
      // SUB(result, in1, in2),
      // AND(result, in1, in2),
      // XOR(result, in1, in2),
      // OR(result, in1, in2)
    )

    shuffle(ops).head
  }

  def makeArithmeticBlock(result: Reg, inputs: List[Reg], length: Int): List[OP] =
    List.fill(length)(makeArithmeticOp(result, inputs))

  def assemble(ops: List[OP]): Map[Uint, OP] = {
    (ops ::: List(DONE)).zipWithIndex.map{ case(op, idx) =>
      (Uint(idx*4), op)
    }.toMap
  }


  def printUpdate(old: MachineState, next: MachineState): Unit = {
    val memUpdateOld = old.mem.toSet.diff(next.mem.toSet).toList.map{
      case(addr, word) => s"mem at ${addr} <- ${word}" }
    val memUpdateNext = next.mem.toSet.diff(old.mem.toSet).toList.map{
      case(addr, word) => s"mem at ${addr} <- ${word}" }


    val regUpdateOld = old.regs.toSet.diff(next.regs.toSet).toList.map{
      case(addr, word) => s"${addr} <- ${word}" }
    val regUpdateNext = next.regs.toSet.diff(old.regs.toSet).toList.map{
      case(addr, word) => s"${addr} <- ${word}" }


    val pcUpdate = s"PC change from ${old.pc} to ${next.pc}"

    // lol
    // this is actually spacemacs preferred indentation
    // matches the code quality I suppose
    val update =
      (if(!memUpdateOld.isEmpty){
         s"mem update\nFrom\t${memUpdateOld.head}\nTo\t${memUpdateNext.head}"
       }
       else "") ++
        (if(!regUpdateOld.isEmpty){
           s"regs update\nFrom\t${regUpdateOld.head}\nTo\t${regUpdateNext.head}\n"
         }
         else "") ++
        pcUpdate

    println(update)
  }

  def printProgram(p: List[OP]): String = {
    assemble(p).toList.sortBy(_._1).map{ case(address, op) =>
      s"$address\t - ${op.show}"
    }.mkString(
      "--- PROGRAM -------------------------\n",
      "\n",
      "\n--- END -----------------------------\n")
  }

  def printRegs(r: Map[Int, Uint]): String = {
    r.toList.sortBy(_._1).map{ case(reg, dword) =>
      s"x${reg}\t - ${dword.toLong.toHexString}"
    }.mkString(
      "--- REGISTERS -----------------------\n",
      "\n",
      "\n--- END -----------------------------\n")
  }

  def printMem(r: Map[Uint, Uint]): String = {
    r.toList.sortBy(_._1).map{ case(reg, dword) =>
      s"address x${reg}\t - ${dword.toLong.toHexString}"
    }.mkString("\n")
  }

}
