package y2021.day04

import zio._
import zio.Console._
import zio.stream.ZStream

import zio.stream.ZPipeline
import java.io.IOException
import y2021.LineStream
import scala.annotation.tailrec

object App2 extends App with LineStream {

  private def parseInt(line: String, sep: String = ","): List[Int] =
    line.trim.split(sep).map(_.trim.toInt).toList

  def buildBingo(sample: Boolean): ZIO[Any, IOException, Bingo] =
    lineStream(4, sample)
      .fold(Bingo(Nil, Nil, None)) { case (bingo, line) =>
        if (bingo.numbers.isEmpty) bingo.copy(numbers = parseInt(line))
        else {
          if (line.isBlank()) bingo.copy(boards = bingo.boards :+ Board(Nil))
          else {
            val board = bingo.boards.last
            val updatedBoard =
              board.copy(lines = board.lines :+ parseInt(line, "\\s+"))

            bingo.copy(boards = bingo.boards.dropRight(1) :+ updatedBoard)
          }
        }
      }

  def playUntilLastWin(bingo: Bingo) = ZStream
    .range(1, bingo.numbers.length + 1)
    .runFoldWhile(bingo)(_.winner.isEmpty) { case (bingo, index) =>
      val drawn = bingo.numbers.take(index).sorted

      def wins(board: Board) = board.linesOrColumns.exists { line =>
        drawn.toSet.intersect(line.toSet) == line.toSet
      }

      bingo.boards
        .find(wins) match {
        case Some(winner) =>
          val remainingBoards = bingo.boards.filterNot(wins)
          if (remainingBoards.isEmpty)
            bingo.copy(winner = Some(winner, index))
          else
            bingo.copy(boards = remainingBoards)
        case None => bingo
      }
    }

  def score(board: Board, numbers: Set[Int], last: Int): Int =
    board.lines
      .map(_.toList)
      .reduce(_ ++ _)
      .filterNot(numbers)
      .reduce(_ + _) * last

  def program(sample: Boolean = false) = for {
    bingo <- buildBingo(sample)
    winningBingo <- playUntilLastWin(bingo)
    _ <- winningBingo.winner match {
      case Some((board, turn)) =>
        printLine(
          score(board, bingo.numbers.take(turn).toSet, bingo.numbers(turn - 1))
        )
      case None =>
        printLine("No winner")
    }

  } yield ()
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program(false).exitCode

}
