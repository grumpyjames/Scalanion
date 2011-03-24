package org.grumpysoft.actioncards

import org.grumpysoft._

object CouncilRoomSpec extends ActionCardSpecBase {

  val stacks = Stacks(mixOfAllTypes, twoCoppers, List())

  val playerTwoStacks = Stacks.deckOnly(witchAndDuchy)
  val playerThreeStacks = Stacks.deckOnly(twoCoppers)

  val table = makeTable(playerTwoStacks, playerThreeStacks)

  val actionResult = CouncilRoom().play(stacks, playerOne, supply, table)

  "council room" should {

    "add four cards" in {
      actionResult.stacks.hand must_==(mixOfAllTypes.take(4) ++ twoCoppers)
    }
    "add a buy" in {
      actionResult.buys must_==1
    }
    "add a card to each other player" in {
      actionResult.table.head._1.hand must_==witchAndDuchy.take(1)
      actionResult.table.last._1.hand must_==twoCoppers.take(1)
    }
    "tell each player what their hand now is" in {
      there was one(playerOne).newHand(actionResult.stacks.hand)
      there was one(playerTwo).newHand(actionResult.table.head._1.hand)
      there was one(playerThree).newHand(actionResult.table.last._1.hand)
    }
  }



}
