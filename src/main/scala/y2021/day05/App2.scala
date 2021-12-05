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

  val program = lineStream(5, true)
    .collect { case pattern(x2, y2, x1, y1) =>
      (x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
    .filter { case (x1, y1, x2, y2) =>
      x1 == x2 || y1 == y2 || angle45(x1, x2, y1, y2)
    }
    .fold(Map.empty[(Int, Int), Int].withDefaultValue(0)) {
      case (map, (x1, y1, x2, y2)) =>
        (if (x1 == x2 || y1 == y2) {
           for {
             x <- x1 to x2 by (if (x1 > x2) -1 else 1)
             y <- y1 to y2 by (if (y1 > y2) -1 else 1)
           } yield (x, y)
         } else {
           val b = (y2 - y1).toDouble / (x2 - x1).toDouble
           for {
             x <- x1 to x2 by (if (x1 > x2) -1 else 1)
           } yield (
             x,
             (y1 + b * (x - x1)).round.toInt
           )
         }) .foldLeft(map) { case (map, point) =>
          map + (point -> (map(point) + 1))
        }
    }
    .tap { map =>
      val buf = ListBuffer.empty[String]
      for (y <- 0 to 9) {
        for (x <- 0 to 9) {
          buf.addOne(map.get((x, y)).map(n => s"$n").getOrElse("."))
        }
        buf.addOne("\n")
      }
      print(buf.mkString)
    }
    .map { map =>
      map.values.count(_ > 1)
    }

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    program.flatMap(res => printLine(s"$res"))

}
