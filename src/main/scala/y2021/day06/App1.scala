package y2021.day06

import zio._
import zio.Console._
import zio.stream.ZStream

import Data._

object App1 extends ZIOAppDefault {

  def step(value: Int): List[Int] =
    value match {
      case 0 => List(6, 8)
      case n => List(n - 1)
    }

  def loop(
      stream: ZStream[Any, Nothing, Int],
      n: Int
  ): ZStream[Any, Nothing, Int] = n match {
    case 0 => stream
    case n => loop(stream.mapConcat(step), n - 1)
  }

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    for {
      count <- loop(ZStream.from(sample), 80).runCount
      _ <- printLine(count)
    } yield ()

}
