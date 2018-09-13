package Ov1
import chisel3._
import chisel3.iotesters._
import org.scalatest.{Matchers, FlatSpec}

import cats.effect._
import cats.effect.IO
import fs2._

import testUtils._

import java.io.File
import java.nio.file.{ Path, Paths }

import atto._, Atto._
import cats.implicits._

object testParser {

  def getListOfFiles(dir: String): List[File] =
    (new File(dir)).listFiles.filter(_.isFile).toList

  def getListOfFiles(dir: Path): List[File] =
    dir.toFile().listFiles.filter(_.isFile).toList


  def getListOfFolders(dir: String): List[File] =
    (new File(dir)).listFiles.filter(_.isDirectory).toList

  def getListOfFolders(dir: Path): List[File] =
    dir.toFile().listFiles.filter(_.isDirectory).toList

  def hexToInt(s: String): BigInt = {
    s.toList.map(x => BigInt("0123456789abcdef".indexOf(x))).reduceLeft(_ * 16 + _)
  }

  val addressParser: Parser[BigInt] =
    count(8, hexDigit).map(_.mkString).flatMap { s =>
      try ok(hexToInt(s)) catch { case e: NumberFormatException => err(e.toString) }
    }


  val instructionParser: Parser[testUtils.Instruction] = for {
    _ <- int
    _ <- char(':')
    _ <- many(char(' '))
    _ <- many(char('	'))
    d <- addressParser
    _ <- many(char(' '))
    _ <- many(char('	'))
    s <- takeRest.map(_.mkString)
  } yield (testUtils.Instruction(d,s))

  // format is "addr:   001122FF       add ra,ra,ra
  // val parseInstruction: Parser[Instruction] = {
  //   ///
  //   ???
  // }

  // def parseInstruction(instruction: String): Instruction = {
  //   ???
  // }

  def parseTest[F[_]:Sync](name: String): F[Unit] = {
    ???
  }

  // def fahrenheitToCelsius(f: Double): Double =
  //   (f - 32.0) * (5.0/9.0)

  // def converter[F[_]](implicit F: Sync[F]): F[Unit] =
  //   io.file.readAll[F](Paths.get("testdata/fahrenheit.txt"), 4096)
  //     .through(text.utf8Decode)
  //     .through(text.lines)
  //     .filter(s => !s.trim.isEmpty && !s.startsWith("//"))
  //     .map(line => fahrenheitToCelsius(line.toDouble).toString)
  //     .intersperse("\n")
  //     .through(text.utf8Encode)
  //     .through(io.file.writeAll(Paths.get("testdata/celsius.txt")))
  //     .compile.drain


}
