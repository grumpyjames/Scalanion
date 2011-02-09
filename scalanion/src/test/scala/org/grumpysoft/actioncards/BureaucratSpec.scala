package org.grumpysoft.actioncards

import org.grumpysoft.Stacks._
import org.grumpysoft.ActionCards.Bureaucrat
import org.grumpysoft.TreasureCards.Silver

object BureaucratSpec extends ActionCardSpecBase {

  val stacks = deckOnly(threeCoppersAndAnEstate)

  "bureaucrat" should {
    "place a silver on top of the player's deck" in {
      val actionResult = Bureaucrat().play(stacks, player)
      actionResult.stacks must_==deckOnly(List(Silver()) ++ threeCoppersAndAnEstate)
      actionResult.treasure must_==0
    }
  }

}