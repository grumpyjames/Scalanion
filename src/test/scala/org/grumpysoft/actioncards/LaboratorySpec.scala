package org.grumpysoft.actioncards

import org.grumpysoft._

object LaboratorySpec extends ActionCardSpecBase {

  val playerOneStacks = Stacks(estateAndDuchy, copperDuchyAndEstate, List())

  val actionResult = Laboratory().play(playerOneStacks, playerOne, supply, eventOnlyTable)

  "laboratory" should {
    "add an action and two cards" in {
      actionResult.actions must_==1
      actionResult.stacks.hand must_==estateAndDuchy ++ copperDuchyAndEstate
    }

    "tell the player what their new hand is" in {
      there was one(playerOne).newHand(actionResult.stacks.hand)
    }
  }

}


