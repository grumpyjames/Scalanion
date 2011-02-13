package org.grumpysoft.actioncards

import org.grumpysoft.ActionCards.Remodel
import org.grumpysoft.{Gain, Stacks, Trash}

object RemodelSpec extends ActionCardSpecBase {

  val playerStacks = Stacks.handOnly(copperAndSilver)

  "remodel" should {
    playerOne.chooseFrom(copperAndSilver, Trash, 1, 1) returns copperAndSilver.drop(1)
    supply.availableCards(5) returns witchAndDuchy
    playerOne.chooseFrom(witchAndDuchy, Gain, 1, 1) returns witchAndDuchy.drop(1)
    supply.buy(witchAndDuchy.last) returns anotherSupply
    val actionResult = Remodel().play(playerStacks, playerOne, supply, eventOnlyTable)
    "ask the player for a card to remodel" in {
      there was one(playerOne).chooseFrom(copperAndSilver, Trash, 1, 1)
    }
    "correctly query the supply for available cards to remodel to" in {
      there was one(supply).availableCards(5)
    }
    "offer the player the available remodel choices from the supply" in {
      there was one(playerOne).chooseFrom(witchAndDuchy, Gain, 1, 1)
    }
    "trash the first chosen card" in {
      (actionResult.stacks.hand ++ actionResult.stacks.discard) must notExist(_.eq(copperAndSilver.last))
    }
    "adds the remodelled card to the discard deck of the player" in {
      actionResult.stacks.discard.head mustEq(witchAndDuchy.last)
    }
    "return the correct supply" in {
      actionResult.supply mustEq(anotherSupply)
    }
    "fire the correct events" in {
      there was one(playerTwo).playerEvent(playerOne, Trash, copperAndSilver.drop(1))
      there was one(playerTwo).playerEvent(playerOne, Gain, witchAndDuchy.drop(1))
    }
  }

}