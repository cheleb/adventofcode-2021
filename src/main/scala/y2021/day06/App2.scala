package y2021.day06

import zio._
import zio.Console._
import zio.stream.ZStream

import Data._

case class Cohort(ages: Map[Int, Int]) {
  def add(i: Int) = copy(ages = ages + (i -> (ages(i) + 1)))
}

object App2 extends ZIOAppDefault {

  def add(ages: Map[Int, Long], i: Int): Map[Int, Long] =
    ages + (i -> (ages(i) + 1))

  def step(ages: Map[Int, Long]): Map[Int, Long] =
    ages.flatMap { case (age, n) =>
      age match {
        case 0 =>
          Map(6 -> (n + ages.getOrElse(7, 0L)), 8 -> n)
        case 7 if (ages.get(0).isDefined) =>
          Map.empty
        case _ =>
          Map((age - 1) -> n)
      }
    }

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
    } yield ()

}
//1650309278600
