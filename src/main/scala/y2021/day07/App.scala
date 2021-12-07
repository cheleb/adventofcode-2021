package y2021.day07

import math._

import Data._
import zio._
import zio.Console._
import zio.stream.ZStream

trait App0 extends ZIOAppDefault {

  val data = sample

  val program = for {
    min <- ZIO.attempt(data.min)
    max <- ZIO.attempt(data.max)
    f <- ZStream
      .range(min, max)
      .zipWithPreviousAndNext
      .collect { case (Some(p), c, Some(n)) =>
        (fuelFor(p), fuelFor(c), fuelFor(n))
      }
      .find { case (p, c, n) =>
        p > c && c < n
      }
      .map { case (_, c, _) => c }
      .runHead
    res <- ZIO.from(f)
    _ <- printLine(res)
  } yield f

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    program

  def fuelFor(x: Int): Long

}
