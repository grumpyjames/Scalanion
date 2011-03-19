package org.grumpysoft.actioncards

import org.grumpysoft._

object LaboratorySpec extends ActionCardSpecBase {

  val playerOneStacks = Stacks(estateAndDuchy, copperDuchyAndEstate, List())

  "laboratory" should {
    "add an action and two cards" in {
      val actionResult = Laboratory().play(playerOneStacks, playerOne, supply, eventOnlyTable)
      actionResult.actions must_==1
      actionResult.stacks.hand must_==estateAndDuchy ++ copperDuchyAndEstate
    }
  }

}


