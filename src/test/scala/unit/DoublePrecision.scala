package unit

case class Precision(val p: Double)

class AlmostEquals(d: Double) {
  def ~=(d2: Double)(implicit p: Precision) = (d - d2).abs <= p.p
}

object DoublePrecision {
  implicit def toAlmostEquals(d: Double) = new AlmostEquals(d)
}