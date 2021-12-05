package y2021.day1

import zio._
import zio.Console._
import zio.stream.ZStream

import zio.stream.ZChannel
import zio.stream.ZPipeline

object App2Sliding extends App {

  val program = for {

    count <- ZStream
      .fromInputStream(
        getClass().getResourceAsStream("/y2021/day1/input.txt")
      )
      .via(ZPipeline.utf8Decode)
      .via(splitLines)
      // .groupBy(_ => ???)
      //  .via(sliding(3))
      .map(_.toInt)
      .zipWithPrevious
      .collect {
        case (Some(p), c) if p < c => ()
      }
      .runCount

    _ <- putStrLn(s"$count")
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program.exitCode

  def splitLines: ZPipeline.WithOut[
    Nothing,
    Any,
    Nothing,
    Any,
    Nothing,
    String,
    ({ type OutEnv[Env] = Env })#OutEnv,
    ({ type OutErr[Err] = Err })#OutErr,
    ({ type OutElem[Elem] = String })#OutElem
  ] =
    new ZPipeline[Nothing, Any, Nothing, Any, Nothing, String] {
      override type OutEnv[Env] = Env
      override type OutErr[Err] = Err
      override type OutElem[Elem] = String
      override def apply[Env, Err, Elem <: String](
          stream: ZStream[Env, Err, Elem]
      )(implicit
          trace: ZTraceElement
      ): ZStream[Env, Err, String] = {
        def next(
            leftover: Option[String],
            wasSplitCRLF: Boolean
        ): ZChannel[Env, Err, Chunk[String], Any, Err, Chunk[String], Any] =
          ZChannel.readWithCause[Env, Err, Chunk[String], Any, Err, Chunk[
            String
          ], Any](
            incomingChunk => {
              val buffer = collection.mutable.ArrayBuffer.empty[String]
              var inCRLF = wasSplitCRLF
              var carry = leftover getOrElse ""

              incomingChunk foreach { string =>
                val concatenated = carry concat string

                if (concatenated.nonEmpty) {

                  // If we had a split CRLF, start reading from the last character of the leftover, which was the '\r'
                  // Otherwise we just skip over the entire previous leftover, as it doesn't contain a newline.
                  val continueFrom =
                    if (inCRLF && carry.nonEmpty) carry.length - 1
                    else carry.length

                  concatenated.zipWithIndex
                    .drop(continueFrom)
                    .foldLeft((0, false, inCRLF)) {
                      case ((sliceStart, skipNext, midCRLF), (char, index)) =>
                        if (skipNext) (sliceStart, false, false)
                        else
                          char match {
                            case '\n' =>
                              buffer += concatenated
                                .substring(sliceStart, index)
                              (index + 1, false, midCRLF)
                            case '\r' =>
                              if (
                                index + 1 < concatenated.length && concatenated(
                                  index + 1
                                ) == '\n'
                              ) {
                                buffer += concatenated
                                  .substring(sliceStart, index)
                                (index + 2, true, false)
                              } else if (index == concatenated.length - 1)
                                (sliceStart, false, true)
                              else (index, false, false)
                            case _ => (sliceStart, false, midCRLF)
                          }
                    } match {
                    case (sliceStart, _, midCRLF) =>
                      carry = concatenated.drop(sliceStart)
                      inCRLF = midCRLF
                  }
                }
              }

              ZChannel.write(Chunk.fromArray(buffer.toArray)) *>
                next(if (carry.nonEmpty) Some(carry) else None, inCRLF)
            },
            halt =>
              leftover match {
                case Some(value) =>
                  ZChannel.write(Chunk.single(value)) *> ZChannel.failCause(
                    halt
                  )
                case None => ZChannel.failCause(halt)
              },
            done =>
              leftover match {
                case Some(value) =>
                  ZChannel.write(Chunk.single(value)) *> ZChannel.succeed(done)
                case None => ZChannel.succeed(done)
              }
          )

        new ZStream[Env, Err, String](
          stream.channel >>> next(None, wasSplitCRLF = false)
        )
      }
    }

  /** Creates a pipeline that takes n elements.
    */
  def sliding[A](n: Long): ZPipeline.WithOut[
    Nothing,
    Any,
    Nothing,
    Any,
    Nothing,
    List[Any],
    ({ type OutEnv[Env] = Env })#OutEnv,
    ({ type OutErr[Err] = Err })#OutErr,
    ({ type OutElem[Elem] = List[Any] })#OutElem
  ] = new ZPipeline[Nothing, Any, Nothing, Any, Nothing, List[Any]] {
    type OutEnv[Env] = Env
    type OutErr[Err] = Err
    type OutElem[Elem] = List[Any]
    override def apply[Env, Err, Elem <: List[Any]](
        stream: ZStream[Env, Err, Elem]
    )(implicit
        trace: ZTraceElement
    ): ZStream[Env, Err, List[Any]] =
      /*
    new ZPipeline[Nothing, Any, Nothing, Any, Nothing, List[Any]] {
      type OutEnv[Env] = Env
      type OutErr[Err] = Err
      type OutElem[Elem] = Elem
      override def apply[Env, Err, Elem <: List[Any]](
          stream: ZStream[Env, Err, Elem]
      )(implicit
          trace: ZTraceElement
      ): ZStream[Env, Err, Elem] =
       */ {
        def next(
            leftover: List[Any]
        ): ZChannel[Env, Err, Chunk[List[Any]], Any, Err, Chunk[
          List[Any]
        ], Any] =
          ZChannel
            .readWithCause[Env, Err, Chunk[List[Any]], Any, Err, Chunk[
              List[Any]
            ], Any](
              incomingChunk => {

                val buffer = collection.mutable.ArrayBuffer
                  .from[Any](leftover)
                incomingChunk.foreach { element =>
                  buffer.addAll(element)
                }

                // while (buffer.length >= n) {}
                ZChannel.write(
                  Chunk.single(buffer.take(n.toInt).toList)
                ) *> next(buffer.drop(n.toInt).toList)

              },
              halt =>
                leftover match {
                  case Nil => ZChannel.failCause(halt)
                  case list =>
                    ZChannel.write(Chunk.single(list)) *> ZChannel
                      .failCause(
                        halt
                      )

                },
              done =>
                leftover match {
                  case Nil => ZChannel.succeed(done)
                  case list =>
                    ZChannel.write(Chunk.single(list)) *> ZChannel
                      .succeed(
                        done
                      )
                }
            )
        new ZStream[Env, Err, List[Any]](stream.channel >>> next(Nil))
      }

  }

}
