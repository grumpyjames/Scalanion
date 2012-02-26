package org.grumpysoft


object Game {
  private def makeStacks(count: Int) : List[Stacks] = {
    List.fill(count)(Stacks.base())
  }

  def apply(players: List[GenericPlayer[Card]], supply: Supply, buyPhase: BuyPhaseFn, actionPhase: ActionPhaseFn) : Game = {
    val allStacks = makeStacks(players.size)
    players.zip(allStacks).foreach(ps => ps._1.newHand(ps._2.hand))
    Game(players, allStacks, supply, buyPhase, actionPhase)
  }

  def standardGame(players: List[GenericPlayer[Card]], supply: Supply) : Game = {
    Game(players, supply, DefaultBuyPhaseFn, DefaultActionPhaseFn)
  }
}

trait BuyPhaseFn {
  def doBuyPhase(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) : (Supply, Seq[Card]);
}

trait ActionPhaseFn {
  type Table = Seq[(Stacks, GenericPlayer[Card])]
  def doActionPhase(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult;
}

private case object DefaultBuyPhaseFn extends BuyPhaseFn {
  def doBuyPhase(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) = {
    BuyPhase(buys, treasure, player, supply)
  }
}

private case object DefaultActionPhaseFn extends ActionPhaseFn {
  def doActionPhase(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) = {
    ActionPhase.doActionPhase(stacks, player, supply, table)
  }
}


// TODO: possibly a misnomer here?
case class Game(players: List[GenericPlayer[Card]],
                allStacks: List[Stacks],
                supply: Supply,
                private val buyPhase: BuyPhaseFn,
                private val actionPhase: ActionPhaseFn)  {

  private case class InnerGame(currentPlayer: GenericPlayer[Card],
                               otherPlayers: List[GenericPlayer[Card]],
                               otherStacks: List[Stacks]) {

    private def nextPlayers(actionResult: ActionResult): List[GenericPlayer[Card]] = {
      actionResult.table.map(_._2).toList ++ List(currentPlayer)
    }

    private def nextStacks(actionResult: ActionResult, finalStacks: Stacks): List[Stacks] = {
      actionResult.table.map(_._1).toList ++ List(finalStacks)
    }

    private def doBuyPhase(treasure: Int, buys: Int, buySupply: Supply, hand: List[Card]) : (Supply, Seq[Card]) = {
      val (newSupply, boughtCards) = buyPhase.doBuyPhase(buys, treasure + Trader.valueHand(hand), currentPlayer, buySupply)
      if (!boughtCards.isEmpty) otherPlayers.foreach(_.playerEvent(currentPlayer, Gain, boughtCards))
      (newSupply, boughtCards)
    }

    def takeTurn(currentStacks: Stacks) : Game = {
      val actionResult = actionPhase.doActionPhase(currentStacks, currentPlayer, supply, otherStacks.zip(otherPlayers))
      actionResult.supply.gameOver() match {
        case true => nextGame(currentPlayer :: otherPlayers, actionResult.stacks :: otherStacks, actionResult.supply)
        case false => {
          val (nextSupply, boughtCards) = doBuyPhase(actionResult.treasure, 1 + actionResult.buys, actionResult.supply, actionResult.stacks.hand)
          val finalStacks = actionResult.stacks.gain(boughtCards).endTurn()
          currentPlayer.newHand(finalStacks.hand)
          nextGame(nextPlayers(actionResult), nextStacks(actionResult, finalStacks), nextSupply)
        }
      }
    }
  }

  private def nextGame(players: List[GenericPlayer[Card]], allStacks: List[Stacks], supply: Supply) = {
    Game(players, allStacks, supply, buyPhase, actionPhase)
  }

  def takeTurn : Game = {
    if (!isOver) InnerGame(players.head, players.tail, allStacks.tail).takeTurn(allStacks.head)
    else this
  }

  def isOver: Boolean = {
    supply.gameOver()
  }

  def leaderboard() : List[(SelfDescribing, Int)] = {
    players.zip(allStacks.map(Scorer.scoreStacks(_))).sortBy(-1 * _._2)
  }

  override def toString: String = {
    players.zip(allStacks).map(a => a._1.describe + " " + a._2.toString).foldLeft("game: ")(_ + "\n" + _)
  }

}
