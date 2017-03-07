package mandelbrot

case class Range(min: Double, max: Double) {
  require(max > min)

  def width = max - min
}
