package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}


object testUtils {

  /**
    Somewhat unintuitively named, a cycle task is a list test tasks at some time step.
    In order to not have to supply a list the scala varargs syntax (*) is used.
    As an example, at step 13 we want to input a value to a signal in: (PeekPokeTester[T] => Unit)
    and check an output out: ((PeekPokeTester[T] => Unit) with the possibility of test failure exception)
    Thanks to varargs syntax this would be
    CycleTask[MyModule](13, in, out)

    Sometimes it is convenient to delay a bunch of checks by some set amount of cycles.
    For instance, assume a component needs 10 cycles to set up, but it's more convenient
    to write tests from T = 0, we do that and then call .delay(10) to ensure the T0 for the
    tasks is actually T = 10
    */
  case class CycleTask[T <: Module](step: Int, run: PeekPokeTester[T] => Unit*){

    // :_* is necessary for calling var args with explicit list
    def delay(by: Int) = CycleTask[T](step + by, run:_*)
  }


  case class Program[T <: Module](instructions: Seq[CycleTask[T]]){

    val lastStep = instructions.maxBy(_.step).step
    val instructionsMap = instructions.groupBy(_.step)

    // Shift the starting time of a program
    def delay(by: Int): Program[T] = Program(instructions.map(_.delay(by)))

    // Coalescing means that the instructions will be run "in parallel"
    def coalesce(that: Program[T]) = Program(instructions ++ that.instructions)

    // Run this program then that program
    def sequentially(that: Program[T]) = Program(instructions ++ that.delay(lastStep + 1).instructions)

    def run(c: T): PeekPokeTester[T] = new PeekPokeTester(c) {
        for(ii <- 0 to lastStep){
          println(s"at step: $ii")
          instructionsMap.getOrElse(ii, Nil).foreach(_.run.foreach(t => t(this)))
          step(1)
        }
      }
  }


  // the value is the machine code, the semantic is the corresponding ASM string.
  // example:
  // value: 0x0005a783,
  // semantic lw a5,0(a1)
  case class Instruction(value: BigInt, semantic: String)

  // The value and address of a register.
  // example:
  // value: 0x0000007F
  // address: 12
  case class Register(address: BigInt, value: BigInt)

  // A word in data memory
  case class DWord(address: BigInt, value: BigInt)


  def loadProgram(instructions: List[Instruction]): Program[Tile] =
    Program(
      instructions.zipWithIndex.map { case(instruction, n) =>
        CycleTask[Tile](
          n,
          d => d.poke(d.dut.io.setup, 1),
          d => d.poke(d.dut.io.running, 0),
          d => d.poke(d.dut.io.checkResult, 0),
          d => d.poke(d.dut.io.IMEMWriteData, instruction.value),
          d => d.poke(d.dut.io.IMEMAddress, n + 4),
          )
      })


  def loadMemory(memory: List[DWord]): Program[Tile] =
    Program(
      memory.zipWithIndex.map { case(dword, n) =>
        CycleTask[Tile](
          n,
          d => d.poke(d.dut.io.setup, 1),
          d => d.poke(d.dut.io.running, 0),
          d => d.poke(d.dut.io.checkResult, 0),
          d => d.poke(d.dut.io.DMEMWriteEnable, 1),
          d => d.poke(d.dut.io.DMEMWriteData, dword.value),
          d => d.poke(d.dut.io.DMEMAddress, dword.address),
          _ => println(s"loading word ${dword.value} into (decimal) address ${dword.address}")
          )
      })


  def loadRegisters(regs: List[Register]): Program[Tile] =
    Program(
      regs.zipWithIndex.map { case(reg, n) =>
        CycleTask[Tile](
          n,
          d => d.poke(d.dut.io.setup, 1),
          d => d.poke(d.dut.io.running, 0),
          d => d.poke(d.dut.io.checkResult, 0),
          d => d.poke(d.dut.io.regsWriteEnable, 1),
          d => d.poke(d.dut.io.regsWriteData, reg.value),
          d => d.poke(d.dut.io.regsAddress, reg.address),
          d => println(s"loading register ${reg.value} into register ${reg.address}")
          )
      })

  def checkRegisters(regs: List[Register]): Program[Tile] =
    Program(
      regs.zipWithIndex.map { case(reg, n) =>
        CycleTask[Tile](
          n,
          d => d.poke(d.dut.io.setup, 1),
          d => d.poke(d.dut.io.running, 0),
          d => d.poke(d.dut.io.checkResult, 0),
          d => d.poke(d.dut.io.regsWriteEnable, 0),
          d => d.poke(d.dut.io.regsAddress, reg.address),
          d => println(s"checking if ${reg.address} matches ${reg.value}"),
          d => d.expect(d.dut.io.regsReadData, reg.value, s"expected ${reg.value} at ${reg.address}, got ${d.peek(d.dut.io.regsReadData)}")
          )
      })


  def checkMemory(memory: List[DWord]): Program[Tile] = {
    Program(
      memory.zipWithIndex.map { case(dword, n) =>
        CycleTask[Tile](
          n,
          d => d.poke(d.dut.io.setup, 1),
          d => d.poke(d.dut.io.running, 0),
          d => d.poke(d.dut.io.checkResult, 0),
          d => d.poke(d.dut.io.DMEMWriteEnable, 0),
          d => d.poke(d.dut.io.DMEMAddress, dword.address),
          d => println(s"next cycle checking if MEM[${dword.address}] matches ${dword.value}"),
          )
      }).coalesce(Program(
                    memory.zipWithIndex.map { case(dword, n) =>
                      CycleTask[Tile](
                        n + 1,
                        d => println(s"cycle checking if MEM[${dword.address}] matches ${dword.value}"),
                        d => println(s"MEM[${dword.address}] was ${d.peek(d.dut.io.DMEMReadData)}"),
                        d => d.expect(d.dut.io.DMEMReadData, dword.value, s"\nexpected ${dword.value} at ${dword.address}, got ${d.peek(d.dut.io.DMEMReadData)}")
                        )
                    }))
  }
}
