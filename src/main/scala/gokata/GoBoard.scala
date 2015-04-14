package gokata

class GoBoard(val size: Int, val moves: List[Move], val prevBoard: Option[GoBoard]) {
  def this(size: Int, moves: List[Move]) = this(size, moves, None)
  def this(size: Int) = this(size, Nil, None)

  val width = size
  val height = size
  def isEmpty = moves == Nil

  def validMove(m: Move): Boolean = {
    (m.x >= 0 && m.x < size) && (m.y >= 0 && m.y < size) &&
      (isRightColor(m.stone)) &&
      (!moves.exists(z => z.x == m.x && z.y == m.y))
  }

  def placeStone(m: Move): GoBoard = {
    if (validMove(m)) {
      println("Try placing " + m)
      val newBoard = new GoBoard(size, m :: moves, Some(this))
      val boardAfterCapture = newBoard.capture(m)
      if (isLoop(boardAfterCapture)) {
        println("Loop detected. Move rejected")
        this
      } else if (boardAfterCapture.isSelfCapture(m)) {
        println("Self-capturing. Move rejected")
        this
      } else {
        println("Move accepted")
        boardAfterCapture
      }
    } else {
      println("Invalid move " + m)
      this
    }
  }

  def isRightColor(s: Stone) = {
    moves match {
      case lastAction :: xs => lastAction.stone == s.reverseColor
      case Nil              => s == Black
    }
  }

  def isSelfCapture(m: Move): Boolean = {
    !hasLiberty(findGroup(m))
  }

  def capture(m: Move): GoBoard = {
    def captureGroup(board: GoBoard, group: Set[Move]): GoBoard = {
      if (group.size > 0) {
        if (!board.hasLiberty(group)) {
          println("Captured " + group)
          val newMoves = board.moves diff group.toList
          new GoBoard(board.size, newMoves, this.prevBoard)
        } else {
          board
        }
      } else {
        board
      }
    }

    val stones = findAdjacentStonesDiffColor(m)
    val groups = stones.map(findGroup).toSet

    groups.foldLeft(this)((board, group) => captureGroup(board, group))

  }

  def isLoop(board: GoBoard): Boolean = {
    board.prevBoard match {
      case Some(b1) => b1.prevBoard match {
        case Some(b2) => board == b2
        case None     => false
      }
      case None => false
    }
  }

  def adjacentPositions(x: Int, y: Int): Set[(Int, Int)] = {
    var positions = Set.empty[(Int, Int)]
    if (x > 0) positions = positions + ((x - 1, y))
    if (x < size - 1) positions = positions + ((x + 1, y))
    if (y > 0) positions = positions + ((x, y - 1))
    if (y < size - 1) positions = positions + ((x, y + 1))
    positions
  }

  def adjacent(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
    math.abs(x1 - x2) + math.abs(y1 - y2) == 1
  }

  def findAdjacentStonesSameColor(m: Move): List[Move] = {
    moves.filter(x => x.stone == m.stone && adjacent(x.x, x.y, m.x, m.y))
  }

  def findAdjacentStonesDiffColor(m: Move): List[Move] = {
    moves.filter(x => x.stone != m.stone && adjacent(x.x, x.y, m.x, m.y))
  }

  def findGroup(m: Move): Set[Move] = {
    def identifyGroup(stones: List[List[Move]]): List[List[Move]] = {
      stones match {
        case x :: xs => {
          val newStones = (x.flatMap(s => {
            findAdjacentStonesSameColor(s)
          })).toSet -- stones.flatten.toSet
          if (!newStones.isEmpty) identifyGroup(newStones.toList :: stones)
          else stones
        }
        case Nil => List.empty[List[Move]]
      }
    }
    identifyGroup(List(List(m))).flatten.toSet
  }

  def hasLiberty(x: Int, y: Int): Boolean = {
    val positions = adjacentPositions(x, y)

    (for { p <- positions } yield {
      !(moves.contains(Move(Black, p._1, p._2)) || moves.contains(Move(White, p._1, p._2)))
    }).foldLeft(false)((x, y) => x || y)
  }

  def hasLiberty(stones: Set[Move]): Boolean = {
    stones.exists { x => hasLiberty(x.x, x.y) }
  }

  def canEqual(other: Any) = {
    other.isInstanceOf[GoBoard]
  }

  override def equals(other: Any) = {
    other match {
      case that: GoBoard => that.canEqual(GoBoard.this) && size == that.size && moves.toSet == that.moves.toSet
      case _             => false
    }
  }

  override def hashCode() = {
    val prime = 41
    prime * (prime + size.hashCode) + moves.hashCode
  }

  def showMove(i: Int, j: Int) = {
    val letter =
      if (moves.contains(Move(Black, j, i))) '@'
      else if (moves.contains(Move(White, j, i))) 'O'
      else 'X'
    if (j == size - 1) letter + "\n" else letter
  }
  override def toString() = {
    (for { i <- 0 until size; j <- 0 until size }
      yield (showMove(i, j))).mkString
  }

}