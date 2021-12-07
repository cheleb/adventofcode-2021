package y2021.day07

import math._

import Data._
import zio._
import zio.stream.ZStream

object App1 extends App0 {

  def fuelFor(x: Int) = data.map(i => abs(i - x)).sum

}
