package y2021

import zio.stream._

trait LineStream {

  def bitToInt(bits: String) = Integer.parseInt(bits, 2)

  def filename(day: Int, sample: Boolean = false): String =
    f"day$day%02d/${if (sample) "sample" else "input"}.txt"

  def lineStream(day: Int, sample: Boolean = false) = ZStream
    .fromInputStream(
      getClass().getResourceAsStream(s"/y2021/${filename(day, sample)}")
    )
    .via(ZPipeline.utf8Decode)
    .via(ZPipeline.splitLines)
}
