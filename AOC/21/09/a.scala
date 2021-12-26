// Gavin Gray AOC 21

import scala.io.StdIn.readLine
import scala.collection.immutable

type Map[T] = Vector[Vector[T]]

case class Posn(x:Int, y:Int)

def getCross(p:Posn): Iterable[Posn] =
    Vector[Posn](
      Posn(p.x + 1, p.y),
      Posn(p.x - 1, p.y),
      Posn(p.x, p.y + 1),
      Posn(p.x, p.y - 1))

def mapVal[T](m:Map[T], p:Posn): Option[T] =
  p match
    case Posn(x, y) if x >= 0 && y >= 0
        && y < m.length && x < m(0).length =>
      Some (m(y)(x))
    case _ => None

object A:
  def main(args: Array[String]): Unit =
    var m = io.Source.stdin.getLines()
      .map(_.toVector.map(_.asDigit))
      .toVector;
    var answer = m
      .zipWithIndex.map((v, y) =>
      v.zipWithIndex.filter((num, x) =>
        getCross(Posn(x, y))
          .flatMap(mapVal(m, _)).forall(_ > num)))
      .foldLeft(Vector[Int]())((acc, v) =>
        acc ++: v.map(_._1 + 1)).sum;
    println(answer);
