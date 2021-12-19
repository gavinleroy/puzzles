// Gavin Gray AOC 21

import scala.io.StdIn._
import scala.annotation.tailrec

case class Posn(x: Int, y: Int, z: Int) {
  def -(other: Posn): Posn = Posn(x - other.x, y - other.y, z - other.z)
}

object A {

  /** NOTE this was by far the most confusing part, the steps I used
   *       to find these configurations were:
   *         1. rotate the cube until a different direction was considered +x
   *         2. rotate the cube around this fixed axis to gain the 4 configurations
   * */
  def orientations(s: Set[Posn]): Seq[Set[Posn]] = {
    def explode(pos: Posn): Seq[Posn] = {
      val Posn(x, y, z) = pos
      Seq(
        // fix x and rotate cube
        Posn(x, y, z), Posn(x, -z, y), Posn(x, -y, -z), Posn(x, z, -y),
        // fix -x and rotate cube
        Posn(-x, y, -z), Posn(-x, -z, -y), Posn(-x, -y, z), Posn(-x, z, y),
        // fix y and rotate cube
        Posn(y, -x, z), Posn(y, -z, -x), Posn(y, z, x), Posn(y, x, -z),
        // fix -y and rotate cube
        Posn(-y, x, z), Posn(-y, -x, -z), Posn(-y, -z, x), Posn(-y, z, -x),
        // fix z and rotate cube
        Posn(z, y, -x), Posn(z, x, y), Posn(z, -y, x), Posn(z, -x, -y),
        // fix -z and rotate cube
        Posn(-z, y, x), Posn(-z, x, -y), Posn(-z, -y, -x), Posn(-z, -x, y),
      )
    }
    s.toSeq.map(explode).transpose.map(_.toSet)
  }

  /** For each pair of Posn (p1, p2) <- shuffle(from, to)
   *  we fix the distance to be p2 - p1 and test if there exists
   *  at least 12 points that intersect at the shifted distance
   * */
  def maybeIntersect(from: Set[Posn], to: Set[Posn]): Option[(Set[Posn], Posn)] = {
    val possibilities =
      for {
        all <- orientations(to)
        f <- from
        t <- all
        dir = t - f
        mapped = all.map(_ - dir)
        if (mapped & from).size >= 12
      } yield (mapped ++ from, dir)
    possibilities.headOption
  }

  def pointsAndScannerPosn(seq: Seq[Set[Posn]]): (Set[Posn], Array[Option[Posn]]) = {
    var scanners: Array[Option[Posn]] = Array.fill(seq.length)(None)

    @tailrec
    def loop(remaining: Seq[(Set[Posn], Int)], beacons: Set[Posn]): Set[Posn] = {
      if (remaining.isEmpty) {
        beacons
      } else {
        val matched =
          for {
            (beaconsI, i) <- remaining
            (beaconsT, dir) <- maybeIntersect(beacons, beaconsI)
          } yield (i, beaconsT, dir)
        val (i, beac, dir) = matched.head
        val rem = remaining.filter(_._2 != i)
        scanners.update(i, Some(dir))
        loop(rem, beac)
      }
    }

    // Similar to the example fix scanner 0 to be at (0, 0, 0)
    val set0 +: others = seq.zipWithIndex
    scanners.update(0, Some(Posn(0, 0, 0)))
    (loop(others, set0._1), scanners)
  }

  def parse(inp: Iterator[String]): Seq[Set[Posn]] = {
    def parseGroup(seq: Seq[String]): Set[Posn] = {
      seq.drop(1)
        .map(s => {
              var Array(x, y, z) = s.split(',');
              Posn(x.toInt, y.toInt, z.toInt)
            }).toSet
    }

    if (inp.isEmpty)
      Seq()
    else
      parseGroup(inp.takeWhile(!_.isEmpty).toSeq) +: parse(inp)
  }

  // FIXME come back after the semester and improve the runtime
  def main(args: Array[String]): Unit = {
    var (beacons, _) = pointsAndScannerPosn(parse(io.Source.stdin.getLines()))
    println(beacons.size)
  }
}
