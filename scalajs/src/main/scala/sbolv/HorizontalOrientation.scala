package sbolv

/**
 * Horizontal directions extend rightwards or leftwards. In a circular coordinate system, rightwards maps to clockwise
 * and leftwards to anti-clockwise.
 * 
 * @author Matthew Pocock
 */
sealed trait HorizontalOrientation {
  /**
   * Reverse the direction.
   * 
   * @return the reversed direction.
   */
  def reverse: HorizontalOrientation
}

case object Rightwards extends HorizontalOrientation {
  override def reverse = Leftwards
}

case object Leftwards extends HorizontalOrientation {
  override def reverse = Rightwards
}
