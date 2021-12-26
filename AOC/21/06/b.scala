// Gavin Gray AOC 21

import scala.io.StdIn.readLine
import scala.collection.immutable

object B:
  def main(args: Array[String]): Unit =
    val init = readLine()
      .split(",")
      .map(_.toInt)
      .foldLeft(Vector.fill(9)(0L))(
        (acc: Vector[Long], i: Int) => acc.updated(i, acc(i) + 1))
      val ans: Long = (1 to 256).foldLeft(init)((vec, _) =>
                 val hd +: tl = vec
                 tl.updated(6, tl(6) + hd) :+ hd).sum
    println(ans)
