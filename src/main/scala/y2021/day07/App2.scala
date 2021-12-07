package y2021.day07

import math._

import Data._
import zio._
import zio.Console._
import zio.stream.ZStream

object App2 extends App0 {

  def fuelFor(x: Int) = data.map(i => (1 to abs(i - x)).sum).sum

}
