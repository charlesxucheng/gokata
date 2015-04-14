package gokata

trait Stone {
  def reverseColor: Stone
}

case object Black extends Stone {
  override def reverseColor() = White
}
case object White extends Stone {
  override def reverseColor() = Black
}