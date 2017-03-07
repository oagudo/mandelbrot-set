package mandelbrot

import scala.annotation.tailrec
import scala.collection.parallel.ForkJoinTaskSupport
import scala.math.sqrt

case class Point(x: Double, y: Double)

case class ColorIntensity(intensity : Double) extends AnyVal

object Mandelbrot {

  def getColorIntensityPerPoint(points : List[Point],
                                xMandelbrotRange : Range,
                                yMandelbrotRange : Range) : List[(ColorIntensity, Point)] = {

    val pointsParallel = points.par

    pointsParallel.tasksupport = new ForkJoinTaskSupport(
      new scala.concurrent.forkjoin.ForkJoinPool(Config.NumberThreads))

    val colorIntensityPerPoint = pointsParallel.map(p => {
      val xTrans = Math.linearTransform(p.x, Config.XScreenRange, xMandelbrotRange)
      val yTrans = Math.linearTransform(p.y, Config.YScreenRange, yMandelbrotRange)
      val colorIntensity = getColorIntensity(Point(xTrans, yTrans))
      (ColorIntensity(colorIntensity), p)
    }).toList

    colorIntensityPerPoint
  }

  private val colorIntensities = (1 to Config.MaxIterations).map(i => {(0.3 + 0.7 * (i.toDouble / Config.MaxIterations.toDouble))}).toList

  private def diverge(z : Point, c : Point): Boolean = {
    return (sqrt ((z.x) * (z.x) + (z.y) * (z.y))) > (0.5 + sqrt (0.25 + (sqrt ((c.x) * (c.x) + (c.y) * (c.y)))))
  }

  private def nextSeries (z: Point, c: Point) = Point(z.x * z.x - z.y * z.y + c.x, 2 * z.x * z.y + c.y)

  private def getColorIntensity(c: Point) : Double = {

    @tailrec
    def recur(step: Int, p: Point) : Double = {
      if (step >= Config.MaxIterations) return 0
      val newP = nextSeries(p, c)
      if (diverge(newP, c)) colorIntensities(step) else recur(step + 1, newP)
    }

    recur(0, Point(0,0))
  }

}