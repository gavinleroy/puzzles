// Gavin Gray AOC 21

import scala.io.StdIn.readLine
import scala.collection.immutable
import math.Ordered.orderingToOrdered

type Map[T] = Vector[Vector[T]]

case class Posn(x:Int, y:Int)

def getCross(p:Posn): Iterable[Posn] = {
    Vector[Posn](
      Posn(p.x + 1, p.y),
      Posn(p.x - 1, p.y),
      Posn(p.x, p.y + 1),
      Posn(p.x, p.y - 1))
}

def mapVal[T](m:Map[T], p:Posn): Option[T] = p match {
  case Posn(x, y) if x >= 0 && y >= 0
      && y < m.length && x < m(0).length => {
    Some (m(y)(x))
  }
  case _ => None
}

def flood[T: Ordering](m: Map[T], elem: (T, Posn), delim: T): Set[Posn] = {
  def loop(elem:(T, Posn), acc:Set[Posn]): Set[Posn] = {
    getCross(elem._2)
      .filter(!acc.contains(_))
      .filter(!mapVal(m, _).isEmpty)
      .map((v) => (mapVal(m, v).get, v))
      .filter(_._1 < delim)
      .foldLeft(acc)((acc, nelem) =>
        loop(nelem, acc + nelem._2))
  };
  loop(elem, Set[Posn](elem._2))
}

object B {
  def main(args: Array[String]): Unit = {
    var m = io.Source.stdin.getLines()
      .map(_.toVector.map(_.asDigit))
      .toVector;
    var a = m
      .zipWithIndex.map((v, y) =>
      v.zipWithIndex.map((num, x) =>
        (num, Posn(x, y))))
      .map(_.filter(e =>
             getCross(e._2)
               .flatMap(mapVal(m, _))
               .forall(_ > e._1)))
      .foldLeft(Vector[(Int, Posn)]())(_ ++: _)
      .map(flood(m, _, 9))
      .map(_.size)
      .sorted
      .takeRight(3)
      .product;
    println(a);
  }
}
