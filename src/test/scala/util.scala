package Ov1
import chisel3.iotesters._
import java.io.File
import java.nio.file.Path
import scala.collection.mutable.LinkedHashMap
import spire.math.{UInt => Uint}
import spire.implicits._
import cats.effect.ContextShift
import atto._, Atto._
import RISCVasm._

object utilz {

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


  implicit class UintExt(val self: Uint) {
    def showS: String = s"${self.toInt}"
    def showU: String = hs(self)
    def show(signed: Boolean): String =
      if(signed)
        showS
      else
        showU
  }
}
