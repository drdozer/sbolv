package sbolv


trait EnumNames[T] {
  def nameFor(t: T): String
  def enumFor(s: String): T

  def allNames: Iterable[String]
  def allEnums: Iterable[T]
}

object EnumNames {
  def apply[T](mapping: (T, String)*): EnumNames[T] = new EnumNames[T] {
    private val forwardMap = mapping.toMap
    private val reverseMap = mapping.map(_.swap).toMap

    override def enumFor(s: String) = reverseMap(s)

    override def nameFor(t: T) = forwardMap(t)

    override def allEnums = forwardMap.keys

    override def allNames = reverseMap.keys
  }
}