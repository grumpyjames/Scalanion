package org.grumpysoft

object Game {
  def makeStacks(count: Int) : List[Stacks] = {
    var stacksBuilder : List[Stacks] = List()
    0.until(count).foreach(a => 
      stacksBuilder = Stacks.base :: stacksBuilder
    )
    stacksBuilder
  }

  def apply(players: List[GenericPlayer[Card]]) : Game = {
    Game(players, makeStacks(players.size))
  }
}

case class Game(private val players: List[GenericPlayer[Card]], private val allStacks:List[Stacks])  {

  def takeTurn() : Game = {
    val currentStacks = allStacks.head
    val currentPlayer = players.head
    currentPlayer.newHand(currentStacks.hand)
    new Game(players.drop(1) ++ List(currentPlayer), allStacks.drop(1) ++ List(currentStacks.endTurn))
  }

  override def toString() : String = {
    players.zip(allStacks).map(a => a._1.describe + " " + a._2.toString).foldLeft("game: ")(_ + "\n" + _)
  }

}
