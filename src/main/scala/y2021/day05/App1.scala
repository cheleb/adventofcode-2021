package y2021.day05

import zio._
import zio.Console._
import y2021.LineStream

object App1 extends ZIOAppDefault with LineStream {

  val pattern = """(\d+),(\d+)\s+->\s+(\d+),(\d+)""".r

  val program = lineStream(5)
    .collect { case pattern(x1, y1, x2, y2) =>
      (x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
    .filter { case (x1, y1, x2, y2) =>
      x1 == x2 || y1 == y2
    }
    .fold(Map.empty[(Int, Int), Int].withDefaultValue(0)) {
      case (map, (x1, y1, x2, y2)) =>
        (for {
          x <- x1 to x2 by (if (x1 > x2) -1 else 1)
          y <- y1 to y2 by (if (y1 > y2) -1 else 1)
        } yield (x, y)).foldLeft(map) { case (map, point) =>
          map + (point -> (map(point) + 1))
        }
    }
    .map { map =>
      map.values.count(_ > 1)
    }

  override def run: ZIO[Environment with ZEnv with ZIOAppArgs, Any, Any] =
    program.flatMap(res => printLine(s"$res"))

}
