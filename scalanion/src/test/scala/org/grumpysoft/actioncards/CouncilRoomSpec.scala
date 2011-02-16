package org.grumpysoft.actioncards

import org.grumpysoft._

object CouncilRoomSpec extends ActionCardSpecBase {

  val stacks = Stacks(mixOfAllTypes, twoCoppers, List())

  val playerTwoStacks = Stacks.deckOnly(witchAndDuchy)
  val playerThreeStacks = Stacks.deckOnly(twoCoppers)

  val table = makeTable(playerTwoStacks, playerThreeStacks)

  "council room" should {
    val actionResult = CouncilRoom().play(stacks, playerOne, supply, table)
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
  }



}
