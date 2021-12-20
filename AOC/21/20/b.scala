// Gavin Gray AOC 21

import scala.io.StdIn._

type Image[T] = Vector[Vector[T]]

case class Posn(x: Int, y: Int):
  def +(other: Posn): Posn = Posn(x + other.x, y + other.y)

val MAX = Integer.parseInt("111111111", 2)

case class Store(img: Image[Boolean], edges: Boolean, ext: Vector[Boolean]):
  // The key is to buffer the image by one (one each side) for
  // every round of the correction. Then, the outer values will change
  // based on if they are all lit or all dark.
  def correct(iters: Int): Store =
    if iters == 0 then
      return this
    val boarder = Vector.fill(img(0).size + 2)(edges)
    val n_img = boarder +: img.map(edges +: _ :+ edges) :+ boarder
    val nn_img =
      (for  y <- 0 until n_img.length
       yield (for  x <- 0 until n_img(y).length
          yield (getValue(n_img, Posn(x, y), edges, ext))
       ).toVector
      ).toVector
    Store(nn_img, ext(if edges then MAX else 0), ext).correct(iters - 1)

  def countDark: Int =
    img.foldLeft(0){
      (acc, row) => acc + row.foldLeft(0)
        {(acc2, v) => acc2 + toi(v)}}

def getValue(img: Image[Boolean], p: Posn, d: Boolean, ext: Vector[Boolean]): Boolean =
  val idx =
    window(p).map(getOr(img, _, d))
      .reverse.zipWithIndex.foldLeft(0){
        (acc, t) => acc + toi(t._1) * scala.math.pow(2, t._2).toInt}
  ext(idx)

def toi(b: Boolean): Int = if b then 1 else 0

def window(p: Posn) =
  for o <- (for
              y <- -1 to 1
              x <- -1 to 1
         yield Posn(x, y))
  yield (p + o)

def getOr[T](vec: Image[T], p: Posn, or: T): T =
  if p.y < 0 || p.x < 0 || p.y >= vec.length || p.x >= vec(p.y).length then
    or
  else vec(p.y)(p.x)

object B:
  def parse(s: String): Vector[Boolean] =
    s.map(_ == '#').toVector

  def main(args: Array[String]): Unit =
    val m = io.Source.stdin.getLines()
    val algo = parse(m.next())
    m.drop(1) // skip middle line
    val numDark = Store(
      m.map(parse).toVector, false, algo
    ).correct(50).countDark
    println(numDark)
