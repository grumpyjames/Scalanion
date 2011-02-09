package org.grumpysoft.actioncards

import org.grumpysoft.Stacks._
import org.grumpysoft.ActionCards.Bureaucrat
import org.grumpysoft.TreasureCards.Silver
object BureaucratSpec extends ActionCardSpecBase {

  val stacks = deckOnly(threeCoppersAndAnEstate)

  "bureaucrat" should {
    "place a silver (from the supply) on top of the player's deck" in {
      supply.available(Silver()) returns true
      supply.buy(Silver()) returns anotherSupply

      val actionResult = Bureaucrat().play(stacks, player, supply)

      there was one(supply).buy(Silver())
      actionResult.supply mustEq(anotherSupply)
      actionResult.stacks must_==deckOnly(List(Silver()) ++ threeCoppersAndAnEstate)
      actionResult.treasure must_==0
    }

  }

}