package org.grumpysoft.actioncards

import org.grumpysoft.ActionCards._
import org.grumpysoft.TreasureCards._
import org.grumpysoft.Trash

import org.grumpysoft.Stacks

object ChapelSpec extends ActionCardSpecBase {

  val stacks = Stacks(List(), silverRemodelAndTwoCoppers, List())

  "chapel" should {
    "offer the player the chance to trash up to four cards from their current hand" in {
      playerOne.chooseFrom(silverRemodelAndTwoCoppers, Trash, 0, 4) returns List()
      val actionResult = Chapel().play(stacks, playerOne, supply, emptyTable)
      actionResult.stacks must_==(stacks)
      actionResult.treasure must_==0
    }

    "trash the selected cards" in {
      playerOne.chooseFrom(silverRemodelAndTwoCoppers, Trash, 0, 4) returns copperAndSilver
      val actionResult = Chapel().play(stacks, playerOne, supply, emptyTable)
      actionResult.stacks.hand must_==List(Copper(), Remodel())
      actionResult.treasure must_==0
    }
  }

}