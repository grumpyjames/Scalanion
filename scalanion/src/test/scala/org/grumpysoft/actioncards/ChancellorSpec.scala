package org.grumpysoft.actioncards

import org.grumpysoft.DeckDiscard
import org.grumpysoft.Stacks
import org.grumpysoft.DiscardYourDeck


object ChancellorSpec extends ActionCardSpecBase {

  val stacks = Stacks(threeCoppersAndAnEstate, twoCoppers, oneRemodel)

  "chancellor" should {
    "give player the option to discard their current deck, and do so if they request it" in {
      playerOne.query(DiscardYourDeck) returns true
      val actionResult = Chancellor().play(stacks, playerOne, supply, eventOnlyTable)
      actionResult.treasure must_==2
      actionResult.stacks must_==Stacks(List(), twoCoppers, oneRemodel ++ threeCoppersAndAnEstate)
      checkEventReceived(playerOne, DeckDiscard, threeCoppersAndAnEstate, eventOnlyTable.map(_._2))
    }

    "just yield +2 treasure if deck is not elected for discard" in {
      playerOne.query(DiscardYourDeck) returns false
      val actionResult = Chancellor().play(stacks, playerOne, supply, eventOnlyTable)
      actionResult.treasure must_==2
      actionResult.stacks mustEq(stacks)
    }

  }
}