// Gavin Gray AOC 21

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.io.StdIn.readLine

type Map[T] = Vector[Vector[T]]
type Posn = (Int, Int)

object B {

  def idx[T](map: Map[T], p: Posn): T = map { p._2 } { p._1 }

  def set[T](map: Map[T], p: Posn, v: T): Map[T] =
    map.updated(p._2, map { p._2 }.updated(p._1, v))

  def add(p1: Posn, p2: Posn): Posn = (p1._1 + p2._1, p1._2 + p2._2)

  def neighbors[T](map: Map[T], p: Posn): Vector[Posn] = {
    Vector((-1, 0), (1, 0), (0, -1), (0, 1))
      .map(add(p, _))
      .filter { (v) =>
        {
          var (x, y) = v
          x >= 0 && y >= 0 && x < map { 0 }.length && y < map.length
        }
      }
  }

  def dijkstra(map: Map[Int], src: Posn, tgt: Posn): Long = {
    var vs = HashSet[Posn]()
    var dist =
      Vector.fill(map.length)(Vector.fill(map { 0 }.length)(Long.MaxValue))
    var next = HashSet[Posn]()

    @tailrec def loop(
        next: HashSet[Posn],
        ds: Map[Long],
        vs: HashSet[Posn]
    ): Long = {

      var p = next.min(Ordering.by(idx(ds, _)))
      if (p == tgt)
        return idx(ds, p)

      var (n_ds, nn_next) = neighbors(map, p).foldLeft((ds, next - p)) {
        (acc, posn) =>
          if (vs.contains(posn))
            acc
          else
            (
              (if (idx(acc._1, p) + idx(map, posn) < idx(acc._1, posn))
                 set(acc._1, posn, idx(acc._1, p) + idx(map, posn))
               else acc._1),
              acc._2 + posn
            )
      }
      loop(nn_next, n_ds, vs + p)
    }

    loop(next + src, set(dist, src, 0), vs)
  }

  def main(args: Array[String]): Unit = {
    var m = io.Source.stdin.getLines().map(_.toVector.map(_.asDigit)).toVector;
    // FIXME this is not going to be the fastest
    var big = Vector
      .tabulate(5, 5)(_ + _)
      .map(_.map(add => m.map(_.map(v => (v + add - 1) % 9 + 1))))
      .flatMap(_.transpose.map(_.flatten))
    println(dijkstra(big, (0, 0), (big.length - 1, big { 0 }.length - 1)))
  }
}
