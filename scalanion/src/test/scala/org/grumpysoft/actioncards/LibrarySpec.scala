package org.grumpysoft.actioncards

import org.grumpysoft.{ActionResult, Stacks}

object LibrarySpec extends ActionCardSpecBase {

  val noActionsDeckStacks = Stacks(copperEstateAndGold, fourCardHand, List())
  val mixedDeckStacks = Stacks(mixOfAllTypes, twoCoppers, List())

  def playLibrary(stacks: Stacks): ActionResult = {
    Library().play(stacks, playerOne, supply, emptyTable)
  }

  "library" should {
    "deal until seven cards are in the hand" in {
      val actionResult = playLibrary(noActionsDeckStacks)
      actionResult.treasure must_==0
      actionResult.stacks.hand must_==fourCardHand ++ copperEstateAndGold
      actionResult.stacks.deck must_==List()
    }

    "discard any action cards dealt in the process" in {
      val actionResult = playLibrary(mixedDeckStacks)
      actionResult.stacks.hand must_==twoCoppers ++ withoutActions(mixOfAllTypes)
      actionResult.stacks.discard must_==actionsOf(mixOfAllTypes)
    }
  }

}