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
}

case object Upwards extends VerticalOrientation {
  override def reverse = Downwards
}

case object Downwards extends VerticalOrientation {
  override def reverse = Upwards
}