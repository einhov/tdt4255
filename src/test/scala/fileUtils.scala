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

object fileUtils {

  def say(word: Any)(implicit filename: sourcecode.File, line: sourcecode.Line): Unit = {
    val fname = filename.value.split("/").last
    println(Console.YELLOW + s"[${fname}: ${sourcecode.Line()}]" + Console.RESET + s" - $word")
  }

  def getListOfFiles(dir: String): List[File] =
    (new File(dir)).listFiles.filter(_.isFile).toList

  def getListOfFiles(dir: Path): List[File] =
    dir.toFile().listFiles.filter(_.isFile).toList


  def getListOfFolders(dir: String): List[File] =
    (new File(dir)).listFiles.filter(_.isDirectory).toList

  def getListOfFolders(dir: Path): List[File] =
    dir.toFile().listFiles.filter(_.isDirectory).toList

  import cats.effect.IO
  import cats.implicits._
  import fs2.{io, text}
  import java.nio.file.Paths
  import java.util.concurrent.Executors
  import scala.concurrent.ExecutionContext

  def relativeFile(name: String) = {
    new File(getClass.getClassLoader.getResource(name).getPath)
  }

  def run(filename: String): IO[List[asmOP]] = {

    val EC = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
    implicit val cs = IO.contextShift(EC)
    import scala.concurrent.ExecutionContext
    import RISCVparser._

    val annoyingTabCharacter = '	'

    io.file.readAll[IO](relativeFile(filename).toPath(), EC, 4096)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(s => !s.trim.isEmpty && !s.startsWith("//"))
      .through(_.map(_.replace(annoyingTabCharacter, ' ')))
      .through(_.map{ x => parseOp.parse(x).done.option })
      .compile.toList.map(_.flatten)
  }
}
