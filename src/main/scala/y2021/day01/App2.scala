package y2021.day01

import zio._
import zio.Console._
import zio.stream.ZStream

import zio.stream.ZChannel
import zio.stream.ZPipeline
import y2021.LineStream

object App2 extends App with LineStream {

  val program = for {

    count <- lineStream(1)
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
