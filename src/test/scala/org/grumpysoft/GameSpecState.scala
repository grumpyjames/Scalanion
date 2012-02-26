package org.grumpysoft

import org.specs2.specification.Scope
import org.specs2.matcher.MustMatchers._
import org.grumpysoft.TreasureCards._

trait GameSpecState extends Scope {
  case class BoringBuyPhase() extends BuyPhaseFn {
    def doBuyPhase(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) = {
      (supply, Nil)
    }
  }

  case class BoringActionPhase() extends ActionPhaseFn {
    def doActionPhase(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
      ActionResult.noTreasureOrBuysOrActions(stacks, supply, table)
    }
  }

  def defaultDeck() = { Copper().times(10) }

  def deckOnly : Stacks = {
    Stacks.deckOnly(defaultDeck())
  }

  def checkStandardHand(player: SinkPlayer) = {
    player.hands.flatten.groupBy(_.describe()).map(a => (a._1, a._2.size)).toList.sortBy(_._2) must_==List(("Estate", 3),("Copper",7))
  }
}