package org.grumpysoft.actioncards

import org.grumpysoft._

object MarketSpec extends ActionCardSpecBase {

  val playerOneStacks = Stacks.deckOnly(copperDuchyAndEstate)

  "market" should {
    "add a buy, an action, a card, and a treasure" in {
      val actionResult = Market().play(playerOneStacks, playerOne, supply, eventOnlyTable)
      actionResult.treasure must_==1
      actionResult.buys must_==1
      actionResult.actions must_==1
      actionResult.stacks.hand must_==copperDuchyAndEstate.take(1)
    }
  }


}

object Market {
  def apply() : Market = { new Market }
}

class Market extends ActionCard(5) {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult(1, 1, 1, stacks.addCards(1), supply, table)
  }

  def describe() = "Market"
}