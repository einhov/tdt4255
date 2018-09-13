package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import testUtils._

// The tautology wizard casts a spell on your tests!
class TestHarnessSpec extends FlatSpec with Matchers {

  val instructions = List[testUtils.Instruction](
    testUtils.Instruction(0x001080b3, "add	ra,ra,ra"),
    testUtils.Instruction(0x001080b3, "add	ra,ra,ra"),
    testUtils.Instruction(0x00420233, "add	rt,rt,rt"),
    testUtils.Instruction(0x00420233, "add	rt,rt,rt"))

  val mem = List[DWord](
    DWord(0x0 , 0x4),
    DWord(0x4 , 0x40),
    DWord(0x8 , 0xFF00F0),
    DWord(0xF0, 0x4000),
    DWord(0x44, 0x1),
    DWord(0xCE, 0xDEAD))

  val regs = List(
    Register(1, 0x20),
    Register(2, 0x10),
    Register(5, 0xFF),
    Register(4, 0x123),
    Register(3, 0x321))

  val loadRegs = loadRegisters(regs)
  val checkRegs = checkRegisters(regs)

  val loadMem = loadMemory(mem)
  val checkMem = checkMemory(mem)


  // it should "be able to write and read registers in setup mode" in {
  //   iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
  //     loadRegs.sequentially(checkRegs).run(c)
  //   } should be(true)
  // }


  // it should "be able to write and read memory in setup mode" in {
  //   iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
  //     loadMem.sequentially(checkMem).run(c)
  //   } should be(true)
  // }


  it should "be able to write memory, regs and program, then read it back" in {

    iotesters.Driver.execute(() => new Tile(), new TesterOptionsManager) { c =>
      val load = loadMem
        .coalesce(loadRegs)
        .coalesce(loadProgram(instructions))

      val check = checkMem.coalesce(checkRegs)

      load.sequentially(check).run(c)

    } should be(true)
  }
}
