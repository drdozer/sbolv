package sbolv.geom

object Point2 {
  val zero = Point2(0.0, 0.0)
}

case class Point2(x: Double, y: Double) {
  def + (rhs: Point2): Point2 = Point2(x + rhs.x, y + rhs.y)
  def - (rhs: Point2): Point2 = Point2(x - rhs.x, y - rhs.y)
  def * (s: Double): Point2 = Point2(x * s, y * s)
  def * (p: Point2): Point2 = Point2(x * p.x, y * p.y)

  def length: Double = Math.sqrt(x * x + y * y)
  def normal: Point2 = Point2(y = x, x = -y)
  def unit: Point2 = this * (1.0 / length)

  def abs: Point2 = Point2(
    x = Math.abs(x),
    y = Math.abs(y))

  def asSVGTranslate = s"translate($x $y)"
}