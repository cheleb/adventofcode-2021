package y2021.day03

import zio._
import zio.Console._
import zio.stream.ZStream

import zio.stream.ZPipeline

object App1 extends App {

  case class BitCount(values: List[Int], n: Int) {
    def add(str: String): BitCount = {
      if (values.isEmpty)
        copy(
          values = (str.map {
            case '1' => 1
            case _   => 0
          }).toList,
          n = 1
        )
      else
        copy(
          values = (str.zipWithIndex.map {
            case ('1', i) => values(i) + 1
            case (_, i)   => values(i)
          }).toList,
          n = n + 1
        )

    }

    def mid = n / 2

    def max = Integer.parseInt(
      (values.map { c => if (c < n / 2) '0' else '1' }).mkString,
      2
    )
    def min = Integer.parseInt(
      (values.map { c => if (c > n / 2) '0' else '1' }).mkString,
      2
    )
  }

  val program = for {

    count <- ZStream
      .fromInputStream(
        getClass().getResourceAsStream("/y2021/day03/input.txt")
      )
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)
      .fold(BitCount(Nil, 0)) { case (count, str) => count.add(str) }

    _ <- putStrLn(s"${count.max} * ${count.min} = ${count.max * count.min}")
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program.exitCode

}
