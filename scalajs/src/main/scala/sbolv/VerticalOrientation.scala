package sbolv

/**
 * Vertical orientations extend upwards or downwards. In a circular coordinate system, upwards maps out from the centre
 * and downwards towards the centre
 *
 * @author Matthew Pocock
 */
sealed trait VerticalOrientation {
  /**
   * Reverse the direction.
   *
   * @return the reversed direction
   */
  def reverse: VerticalOrientation

  /**
   * +/-1.0, depending on the orientation.
   *
   * @return  a unit vector giving the direction
   */
  def sgn: Double
}

object VerticalOrientation {
  val lowerCaseNames: EnumNames[VerticalOrientation] = EnumNames(Upwards -> "upwards", Downwards -> "downwards")
  val upperCaseNames: EnumNames[VerticalOrientation] = EnumNames(Upwards -> "Upwards", Downwards -> "Downwards")
}

case object Upwards extends VerticalOrientation {
  override def reverse = Downwards
  override def sgn = +1.0
}

case object Downwards extends VerticalOrientation {
  override def reverse = Upwards
  override def sgn = -1.0
}