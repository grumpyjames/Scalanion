package org.grumpysoft.actioncards

import org.grumpysoft.{Discard, ActionResult, Stacks}

object LibrarySpec extends ActionCardSpecBase {

  val noActionsDeckStacks = Stacks(copperEstateAndGold, fourCardHand, List())
  val mixedDeckStacks = Stacks(mixOfAllTypes, twoCoppers, List())

  def playLibrary(stacks: Stacks): ActionResult = {
    Library().play(stacks, playerOne, supply, eventOnlyTable)
  }

  "library" should {
    "deal until seven cards are in the hand" in {
      val actionResult = playLibrary(noActionsDeckStacks)
      actionResult.treasure must_==0
      actionResult.stacks.hand must_==fourCardHand ++ copperEstateAndGold
      actionResult.stacks.deck must_==List()
    }

    val actionResult = playLibrary(mixedDeckStacks)
    val discardedActions = actionsOf(mixOfAllTypes)
    "discard any action cards dealt in the process" in {
      actionResult.stacks.hand must_==twoCoppers ++ withoutActions(mixOfAllTypes)
      actionResult.stacks.discard must_==discardedActions
    }
    "transmit discard events for those action cards" in {
      eventOnlyTable.map(_._2).foreach(otherPlayer =>
        there was one(otherPlayer).playerEvent(playerOne, Discard, discardedActions)
      )
    }
  }

}