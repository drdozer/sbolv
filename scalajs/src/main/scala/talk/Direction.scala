package talk

sealed trait Direction {
  def reverse: Direction
}

case object Rightwards extends Direction {
  override def reverse = Leftwards
}

case object Leftwards extends Direction {
  override def reverse = Rightwards
}
