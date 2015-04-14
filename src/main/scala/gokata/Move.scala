package gokata

case class Move(val stone: Stone, val x: Int, val y: Int) extends Equals {
  def canEqual(other: Any) = {
    other.isInstanceOf[gokata.Move]
  }

  override def equals(other: Any) = {
    other match {
      case that: gokata.Move => that.canEqual(Move.this) && stone == that.stone && x == that.x && y == that.y
      case _ => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime * (prime + stone.hashCode) + x.hashCode) + y.hashCode
  }
}