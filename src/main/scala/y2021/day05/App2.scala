package y2021.day05

import zio._
import zio.Console._
import y2021.LineStream
import scala.collection.mutable.ListBuffer

object App2 extends ZIOAppDefault with LineStream {

  val pattern = """(\d+),(\d+)\s+->\s+(\d+),(\d+)""".r

  def angle45(x1: Int, x2: Int, y1: Int, y2: Int): Boolean = {
    import math._

    val cosa = sqrt(pow(x2 - x1, 2) / (pow(x2 - x1, 2) + pow(y2 - y1, 2)))
    val a = acos(cosa)
    a == math.Pi / 4
  }

  def bidiRange(a: Int, b: Int) =
    a to b by (if (a > b) -1 else 1)

  def program(sample: Boolean = false) = lineStream(5, sample)
    .collect { case pattern(x2, y2, x1, y1) =>
      (x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
    .mapConcat {
      case (x1, y1, x2, y2) if x1 == x2 =>
        for (y <- bidiRange(y1, y2)) yield (x1, y)
      case (x1, y1, x2, y2) if y1 == y2 =>
        for (x <- bidiRange(x1, x2)) yield (x, y1)
      case (x1, y1, x2, y2) =>
        val a = (y2 - y1).toDouble / (x2 - x1).toDouble
        val b = y1
        def f(x: Int) = (b + a * (x - x1)).toInt
        for {
          x <- bidiRange(x1, x2)
        } yield (x, f(x))

    }
    .fold(Map.empty[(Int, Int), Int].withDefaultValue(0)) { case (map, point) =>
      map + (point -> (map(point) + 1))
    }
    .tap { map =>
      ZIO.when(sample) {
        val buf = ListBuffer.empty[String]
        for (y <- 0 to 9) {
          for (x <- 0 to 9) {
            buf.addOne(map.get((x, y)).map(n => s"$n").getOrElse("."))
          }
          buf.addOne("\n")
        }
        print(buf.mkString)
      }
    }
    .map { map =>
      map.values.count(_ > 1)
    }

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    program(false).flatMap(res => printLine(s"$res"))

}
