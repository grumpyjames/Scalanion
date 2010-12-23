package org.grumpysoft

object Game {
  def makeStacks(count: Int) : List[Stacks] = {
    var stacksBuilder : List[Stacks] = List()
    0.until(count).foreach(a => 
      stacksBuilder = Stacks.base :: stacksBuilder
    )
    stacksBuilder
  }
}

import Game._

class Game(val players: List[GenericPlayer[Card]], private val allStacks:List[Stacks])  {

  def this(players: List[GenericPlayer[Card]]) = {
    this(players, makeStacks(players.size))
  }

  def takeTurn() : Game = {
    val currentStacks = allStacks.head
    val currentPlayer = players.head
    currentPlayer.newHand(currentStacks.hand)
    new Game(players.drop(1) ++ List(currentPlayer), allStacks.drop(1) ++ List(currentStacks.end))
  }

  override def toString() : String = {
    players.zip(allStacks).map(a => a._1.describe + " " + a._2.toString).foldLeft("game: ")(_ + "\n" + _)
  }

}
