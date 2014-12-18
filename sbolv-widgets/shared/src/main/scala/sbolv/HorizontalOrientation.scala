package sbolv

/**
 * Horizontal orientations extend rightwards or leftwards. In a circular coordinate system, rightwards maps to clockwise
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

  /**
   * +/-1.0, depending on the orientation.
   *
   * @return  a unit vector giving the direction
   */
  def sgn: Double
}

object HorizontalOrientation {
  val lowerCaseNames: EnumNames[HorizontalOrientation] = EnumNames(Rightwards -> "rightwards", Leftwards -> "leftwards")
  val upperCaseNames: EnumNames[HorizontalOrientation] = EnumNames(Rightwards -> "Rightwards", Leftwards -> "Leftwards")

  implicit val ord: Ordering[HorizontalOrientation] = new Ordering[HorizontalOrientation] {
    override def compare(x: HorizontalOrientation, y: HorizontalOrientation) = (x, y) match {
      case (Leftwards, Leftwards) => 0
      case (Leftwards, Rightwards) => -1
      case (Rightwards, Leftwards) => +1
      case (Rightwards, Rightwards) => 0
    }
  }
}

case object Rightwards extends HorizontalOrientation {
  override def reverse = Leftwards
  override def sgn = +1.0
}

case object Leftwards extends HorizontalOrientation {
  override def reverse = Rightwards
  override def sgn = -1.0
}
