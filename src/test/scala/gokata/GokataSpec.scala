package gokata

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GokataSpec extends FlatSpec with Matchers{

  "1. The board" should "be empty at the start of the game" in {
    val board = new GoBoard(5)
    board.isEmpty should be(true)
  }
  "The first stone to be placed" should "be black" in {
    val board = new GoBoard(5)
    board.placeStone(Move(Black, 1, 2)).moves shouldBe List(Move(Black, 1, 2))
  }
  "The second stone to be placed" should "be white" in {
     val board = new GoBoard(5)
     board.placeStone(Move(Black, 1, 2)).placeStone(Move(White,3,4)).moves shouldBe List(Move(White, 3, 4), Move(Black, 1, 2))
  }
  
  "A stone placed " should "not take occupied point" in {
    val board = new GoBoard(5)
    board.placeStone(Move(Black, 1, 2)).placeStone(Move(White, 1, 2)).moves shouldBe List(Move(Black, 1, 2))
  }
  
  "A stone not next to any other stones" should "be identified as a group" in {
    val board = new GoBoard(5).placeStone(Move(Black, 1, 2))
    board.findGroup(Move(Black, 1, 2)) shouldBe Set(Move(Black, 1, 2))
  }
  
  "A stone next to some other stones" should "be identified as the same group as other stones" in {
    var board = new GoBoard(5).placeStone(Move(Black, 1, 2)).placeStone(Move(White, 2,2)).placeStone(Move(Black, 1, 3))
    board.adjacentPositions(1, 2) shouldBe Set((0, 2), (2, 2), (1, 1), (1, 3))
    board.findAdjacentStonesSameColor(Move(Black, 1, 2)) shouldBe List(Move(Black, 1, 3))
    board.findGroup(Move(Black, 1, 2)) shouldBe Set(Move(Black, 1, 2), Move(Black, 1, 3))

    board = board.placeStone(Move(White, 4,4)).placeStone(Move(Black, 2,3))
    board.findGroup(Move(Black, 1, 2)) shouldBe Set(Move(Black, 1, 2), Move(Black, 1, 3), Move(Black, 2, 3))

    board = new GoBoard(5, List(Move(Black, 0, 0), Move(White, 4, 4), Move(Black, 0, 1), Move(White, 3, 3), 
        Move(Black, 1, 0)))
    board.findGroup(Move(Black, 0, 0)) shouldBe Set(Move(Black, 0, 0), Move(Black, 0, 1), Move(Black, 1, 0))
    
    board = board.placeStone(Move(White, 3, 4)).placeStone(Move(Black, 0, 2))
    board.findGroup(Move(Black, 1, 0)) shouldBe Set(Move(Black, 0, 0), Move(Black, 0, 1), Move(Black, 1, 0), Move(Black, 0, 2))
  }
  
  " A stone played" should "not be self-capturing" in {
    val board = new GoBoard(5, List(Move(Black, 2, 2), Move(Black, 4, 2), Move(Black, 3, 1), Move(Black, 3, 3)))
    val group = board.findGroup(Move(White, 3, 2)) 
    group shouldBe Set(Move(White, 3, 2))
    board.hasLiberty(group) shouldBe false
    board.isSelfCapture(Move(White, 3, 2)) shouldBe true
    
    val board2 = new GoBoard(5).placeStone(Move(Black, 0, 1)).placeStone(Move(White, 4, 4)).placeStone(Move(Black, 1, 0))
    board2.findGroup(Move(White, 0,0)) shouldBe Set(Move(White, 0, 0)) 
    board2.hasLiberty(board2.findGroup(Move(White, 0,0))) shouldBe false
    board2.placeStone(Move(White, 0, 0)) shouldBe board2
  }
  
  it should "remove the captured stones" in {
    val board = new GoBoard(5).placeStone(Move(Black, 1, 1)).placeStone(Move(White, 4, 1)).placeStone(Move(Black, 2, 0))
        .placeStone(Move(White, 4, 2)).placeStone(Move(Black, 2, 2)).placeStone(Move(White, 2, 1))
    val board2 = board.placeStone(Move(Black, 3, 1))
    board2.moves.contains(Move(White, 2, 1)) shouldBe false
  }
  
  it should "not put the game back in the same position as it was on your last turn" in {
    val board = new GoBoard(5).placeStone(Move(Black, 1, 1)).placeStone(Move(White, 3, 0)).placeStone(Move(Black, 2, 0))
        .placeStone(Move(White, 3, 2)).placeStone(Move(Black, 2, 2)).placeStone(Move(White, 4, 1)).placeStone(Move(Black, 3, 1))
        .placeStone(Move(White, 2, 1))
    board.moves.contains(Move(Black, 3, 1)) shouldBe false
    board.placeStone(Move(Black, 3, 1)) shouldBe board
  }
  
}