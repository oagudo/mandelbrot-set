package mandelbrot

object Config {
  val MaxIterations = 100

  val ScreenResolution = (756, 756)

  val XScreenRange = Range(0, ScreenResolution._1)
  val YScreenRange = Range(0, ScreenResolution._2)

  val XMandelbrotRange = Range(-3, 1)
  val YMandelbrotRange = Range(-2, 2)

  val NumberThreads = 8
}