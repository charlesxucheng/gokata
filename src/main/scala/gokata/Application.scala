package gokata

import scala.io.StdIn

object Application {

  def main(args: Array[String]): Unit = {
    var board = new GoBoard(args(0).toInt)
    println(board)
    var stone: Stone = Black
    //board should be empty on creation
    while (true) {
      println("Next Move? - " + stone)
      val line = StdIn.readLine("Enter 2 numbers between 0 and " + args(0) + " separated by a space, or 999 to exit\n" )
      println("You entered: " + line)
      try {
        val coord = line.split(" ").map(_.toInt)
        if (coord(0) == 999) System.exit(0)
        val lastBoard = board
        board = board.placeStone(Move(stone, coord(0), coord(1)))
        println(board)
        if (lastBoard != board) stone = stone.reverseColor
      } catch {
        case e: Exception => println("Invalid input" + e)
      }
    }
  }
}

