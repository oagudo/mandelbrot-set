package mandelbrot

object Math {
  def linearTransform(fromValue : Double, fromRange : Range, toRange : Range): Double = {
    require(fromRange.width != 0)

    val percentage = fromValue * 100.0 / fromRange.width
    val toValue = toRange.width * percentage / 100.0
    toRange.min + toValue
  }
}
