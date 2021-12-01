package y2021.day1

import zio._
import zio.Console._
import zio.stream.ZStream

import zio.stream.ZChannel
import zio.stream.ZPipeline

object App2 extends App {

  val program = for {

    count <- ZStream
      .fromInputStream(
        getClass().getResourceAsStream("/y2021/day1/input.txt")
      )
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)
      .map(_.toInt)
      .zipWithPreviousAndNext
      .collect { case (Some(p), c, Some(n)) => p + c + n }
      .zipWithPrevious
      .collect {
        case (Some(p), c) if p < c => ()
      }
      .runCount

    _ <- putStrLn(s"$count")
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program.exitCode

}
