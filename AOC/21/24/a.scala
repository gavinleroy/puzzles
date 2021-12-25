// Gavin Gray AOC 21

import scala.io.StdIn._
import scala.math.min

// Q: can you do this in scala with Enum?
sealed trait Op
case class Push(i: Int) extends Op
case class Pop(i: Int) extends Op

def solve(ops: Iterator[Op]): Long =
  val ans = Array.fill(14)(0L)
  ops.zipWithIndex.foldLeft(List[(Int, Int)]())( (stack, op) =>
    op._1 match
      case Push(i) => (op._2, i) :: stack
      case Pop(v) =>
        val (pidx, pv) :: tail = stack
        val di = pv + v
        val d = min(9 + di, 9)
        ans(pidx) = d - di
        ans(op._2) = d
        tail)
  ans.foldLeft(0L)(_ * 10 + _)

object A:
  def main(args: Array[String]): Unit =
    val ops = io.Source.stdin.getLines().sliding(18, 18)
      .map(seq =>
             if seq(4).drop(6).take(2) == "1" then
               Push(seq(15).drop(6).take(3).toInt)
             else
               Pop(seq(5).drop(6).take(3).toInt))
    println(solve(ops))
