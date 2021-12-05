package y2021.day03

import zio._
import zio.Console._
import zio.stream.ZStream

import zio.stream.ZPipeline
import java.io.IOException
import y2021.LineStream
import scala.annotation.tailrec

object App2NoStream extends App with LineStream {

  @tailrec
  def select(
      input: List[String],
      p: (Long, Long) => Boolean = _ >= _,
      n: Int = 0
  ): String = {

    val (ones, zeros) =
      input
        .partition { line =>
          line.charAt(n) == '1'
        }
    if (p(ones.length, zeros.length))
      if (ones.length == 1) ones.head
      else select(ones, p, n + 1)
    else if (zeros.length == 1) zeros.head
    else select(zeros, p, n + 1)

  }

  def program(sample: Boolean = false) = for {
    all <- lineStream(3, sample).fold(List.empty[String])(_ :+ _)
    o2 = select(all)
    co2 = select(all, _ < _)
  } yield (bitToInt(o2), bitToInt(co2))

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program().flatMap { case (o2, co2) =>
      printLine(s"$o2  * $co2 = ${o2 * co2}")
    }.exitCode

}
