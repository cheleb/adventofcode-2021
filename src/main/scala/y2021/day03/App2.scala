package y2021.day03

import zio._
import zio.Console._
import zio.stream.ZStream

import zio.stream.ZPipeline
import java.io.IOException
import y2021.LineStream

object App2 extends App with LineStream {

  def select(
      input: ZStream[Any, Throwable, String],
      p: (Int, Int) => Boolean = _ >= _,
      n: Int = 0
  ): ZManaged[Any, Throwable, String] = for {

    split <- input
      .partition(_.charAt(n) == '1')
    _ <- ZIO.debug(s"${split}").toManaged

    ones <- split._1.runFold(List.empty[String])(_ :+ _).toManaged
    _ <- ZIO.debug(s"${split}").toManaged
    zeros <- split._2.runFold(List.empty[String])(_ :+ _).toManaged
    _ <- ZIO.debug(s"${ones.size}").toManaged
    res <-
      if (p(ones.size, zeros.size))
        if (ones.size == 1) ZManaged.from(ones(0))
        else select(ZStream.from(ones), p, n + 1)
      else if (zeros.size == 1) ZManaged.from(zeros(0))
      else select(ZStream.from(zeros), p, n + 1)

  } yield res

  def program(sample: Boolean = false) = for {
    o2 <- select(lineStream(3, sample))
    co2 <- select(lineStream(3, sample), _ < _)
  } yield (bitToInt(o2), bitToInt(co2))

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program().use { case (o2, co2) =>
      printLine(s"$o2  * $co2 = ${o2 * co2}")
    }.exitCode

}
