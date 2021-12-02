package y2021.day2

import zio._
import zio.Console._
import zio.stream.ZStream

import zio.stream.ZPipeline

object App1 extends App {

  val pattern = """(\w+)\s+(\d+)""".r

  case class Pos(x: Int, y: Int)

  val program = for {

    pos <- ZStream
      .fromInputStream(
        getClass().getResourceAsStream("/y2021/day2/input.txt")
      )
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)
      .collect { case pattern(move, value) => (move, value.toInt) }
      .fold(Pos(0, 0)) { case (pos, (move, value)) =>
        println(s"$move $value")
        move match {
          case "forward" => pos.copy(x = pos.x + value)
          case "up"      => pos.copy(y = pos.y - value)
          case "down"    => pos.copy(y = pos.y + value)
        }
      }

    _ <- putStrLn(s"$pos: ${pos.x * pos.y}")
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program.exitCode

}
