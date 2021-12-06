package y2021.day06

import zio._
import zio.Console._
import zio.stream.ZStream

import Data._
import scala.annotation.tailrec

object App2_1 extends ZIOAppDefault {

  def add(ages: Map[Int, Long], i: Int): Map[Int, Long] =
    ages + (i -> (ages(i) + 1))

  def step(ages: Map[Int, Long]): Map[Int, Long] =
    ages
      .filterNot { case (age, _) =>
        age == 7
      }
      .map {
        case (0, n) =>
          (8 -> n)
        case (age, n) =>
          ((age - 1) -> n)
      } + (6 -> (ages.getOrElse(7, 0L) + ages.getOrElse(0, 0L)))

  @tailrec
  def loop(
      ages: Map[Int, Long],
      n: Int
  ): Map[Int, Long] = n match {
    case 0 => ages
    case n => loop(step(ages), n - 1)
  }

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    for {
      count <- ZIO.attempt(
        loop(
          input
            .foldLeft(Map.empty[Int, Long].withDefaultValue(0L))((c, i) =>
              add(c, i)
            ),
          256
        )
      )

      _ <- printLine(count)
      _ <- printLine(count.values.sum)
      _ <- printLine(s"${count.values.sum == 1650309278600L}")
    } yield ()

}
//1650309278600
