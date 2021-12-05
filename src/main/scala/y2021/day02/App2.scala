package y2021.day02

import zio._
import zio.Console._
import zio.stream.ZStream

import zio.stream.ZPipeline
import y2021.LineStream

object App2 extends App with LineStream {

  val pattern = """(\w+)\s+(\d+)""".r

  case class Pos(x: Int, y: Int, aim: Int)

  val program = for {

    pos <- lineStream(2)
      .collect { case pattern(move, value) => (move, value.toInt) }
      .fold(Pos(0, 0, 0)) { case (pos, (move, value)) =>
        move match {
          case "forward" =>
            pos.copy(x = pos.x + value, y = pos.y + pos.aim * value)
          case "up"   => pos.copy(aim = pos.aim - value)
          case "down" => pos.copy(aim = pos.aim + value)
        }
      }

    _ <- putStrLn(s"$pos: ${pos.x * pos.y}")
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program.exitCode

}
