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
