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
    val allStacks = makeStacks(players.size)
    players.zip(allStacks).foreach( ps => ps._1.newHand(ps._2.hand))
    Game(players, allStacks, supply, buyPhase)
  }
}

trait BuyPhaseFn {
  def doBuyPhase(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) : (Supply, Seq[Card]);
}

case class Game(players: List[GenericPlayer[Card]],
                allStacks: List[Stacks],
                supply: Supply,
                private val buyPhase: BuyPhaseFn)  {



  private case class InnerGame(currentPlayer: GenericPlayer[Card],
                               currentStacks: Stacks,
                               otherPlayers: List[GenericPlayer[Card]],
                               otherStacks: List[Stacks]) {
    private def doBuyPhase() : (Supply, Seq[Card]) = {
      val (newSupply, boughtCards) = buyPhase.doBuyPhase(1, Trader.valueHand(currentStacks.hand), currentPlayer, supply)
      if (!boughtCards.isEmpty) otherPlayers.foreach(_.playerEvent(currentPlayer, Gain, boughtCards))
      (newSupply, boughtCards)
    }

    def takeTurn() : Game = {
      val (newSupply, boughtCards) = doBuyPhase()
      val finalStacks = currentStacks.gain(boughtCards).endTurn
      currentPlayer.newHand(finalStacks.hand)
      Game(otherPlayers ++ List(currentPlayer), otherStacks ++ List(finalStacks), newSupply, buyPhase)
    }
  }

  def takeTurn() : Game = {
    InnerGame(players.head, allStacks.head, players.tail, allStacks.tail).takeTurn
  }

  override def toString() : String = {
    players.zip(allStacks).map(a => a._1.describe + " " + a._2.toString).foldLeft("game: ")(_ + "\n" + _)
  }

}
