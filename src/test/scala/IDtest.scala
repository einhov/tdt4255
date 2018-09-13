package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}
import testUtils._


// class CyclicDotSpec extends FlatSpec with Matchers {
//   behavior of "Instruction decode"


//   it should "hello?" in {

//     val dur: Map[String, BigInt] = Map()

//     val ins = (0 to 20).map(ii =>
//       CycleTask[InstructionDecode](
//         ii,
//         d => d.poke(d.dut.io.yo, 0),
//         d => d.poke(d.dut.io.currentIFstage, dur),
//         )
//     )

//     iotesters.Driver.execute(() => new InstructionDecode, new TesterOptionsManager) { c =>
//       IoSpec[InstructionDecode](ins, c).myTester
//     } should be(true)
//   }
// }
