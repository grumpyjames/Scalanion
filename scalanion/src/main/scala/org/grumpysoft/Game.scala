package org.grumpysoft


object Game {
  private def makeStacks(count: Int) : List[Stacks] = {
    var stacksBuilder : List[Stacks] = List()
    0.until(count).foreach(a => 
      stacksBuilder = Stacks.base :: stacksBuilder
    )
    stacksBuilder
  }

  def apply(players: List[GenericPlayer[Card]], supply: Supply, buyPhase: BuyPhaseFn) : Game = {
    Game(players, makeStacks(players.size), supply, buyPhase)
  }
}

trait BuyPhaseFn {
  def doBuyPhase(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) : (Supply, Seq[Card]);
}

case class Game(players: List[GenericPlayer[Card]],
                allStacks:List[Stacks],
                supply: Supply,
                private val buyPhase: BuyPhaseFn)  {

  def takeTurn() : Game = {
    val currentStacks = allStacks.head
    val currentPlayer = players.head
    currentPlayer.newHand(currentStacks.hand)
    val buyResult = buyPhase.doBuyPhase(1, Trader.valueHand(currentStacks.hand), currentPlayer, supply)
    if (!buyResult._2.isEmpty) players.tail.foreach(_.playerEvent(currentPlayer, Gain, buyResult._2))
    new Game(players.drop(1) ++ List(currentPlayer), allStacks.drop(1) ++ List(currentStacks.gain(buyResult._2).endTurn), buyResult._1, buyPhase)
  }

  override def toString() : String = {
    players.zip(allStacks).map(a => a._1.describe + " " + a._2.toString).foldLeft("game: ")(_ + "\n" + _)
  }

}
