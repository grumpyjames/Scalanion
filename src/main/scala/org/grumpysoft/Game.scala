package org.grumpysoft

import collection.immutable.List


object Game {
  private def makeStacks(count: Int) : List[Stacks] = {
    List.fill(count)(Stacks.base())
  }

  def apply(players: List[GenericPlayer[Card]], supply: Supply, buyPhase: BuyPhaseFn, actionPhase: ActionPhaseFn) : Game = {
    val allStacks = makeStacks(players.size)
    val table: List[(GenericPlayer[Card], Stacks)] = players.zip(allStacks)
    table.foreach(ps => ps._1.newHand(ps._2.hand))
    Game(GameState(table, supply), buyPhase, actionPhase)
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

case class GameState(table: List[(GenericPlayer[Card], Stacks)], supply: Supply) {
  val players = table.map(_._1)
  val stacks = table.map(_._2)
}


// TODO: possibly a misnomer here?
// This is actually the state of the game, bundled with some wiring of how it is played.
// perhaps these concerns could be disassociated?
case class Game(state: GameState,
                private val buyPhase: BuyPhaseFn,
                private val actionPhase: ActionPhaseFn)  {

  private val players = state.players
  private val stacks = state.stacks

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
      val actionResult = actionPhase.doActionPhase(currentStacks, currentPlayer, state.supply, otherStacks.zip(otherPlayers))
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
    Game(GameState(players.zip(allStacks), supply), buyPhase, actionPhase)
  }

  def takeTurn : Game = {
    if (!isOver) InnerGame(players.head, players.tail, stacks.tail).takeTurn(stacks.head)
    else this
  }

  def isOver: Boolean = {
    state.supply.gameOver()
  }

  def leaderboard() : List[(SelfDescribing, Int)] = {
    players.zip(stacks.map(Scorer.scoreStacks(_))).sortBy(-1 * _._2)
  }

  override def toString: String = {
    players.zip(stacks).map(a => a._1.describe + " " + a._2.toString).foldLeft("game: ")(_ + "\n" + _)
  }

}
