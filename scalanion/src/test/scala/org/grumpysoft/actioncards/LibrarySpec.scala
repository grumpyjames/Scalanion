package org.grumpysoft.actioncards

import org.grumpysoft.{Discard, ActionResult, Stacks}

object LibrarySpec extends ActionCardSpecBase {

  val noActionsDeckStacks = Stacks(copperEstateAndGold, fourCardHand, List())
  val mixedDeckStacks = Stacks(mixOfAllTypes, twoCoppers, List())
  val evenMoreMixedDeckStacks = Stacks(slightlyDifferentMix, twoCoppers, List())

  def playLibrary(stacks: Stacks): ActionResult = {
    Library().play(stacks, playerOne, supply, eventOnlyTable)
  }

  "library" should {
    "deal until seven cards are in the hand" in {
      val actionResult = playLibrary(noActionsDeckStacks)
      actionResult.treasure must_==0
      actionResult.stacks.hand must_==fourCardHand ++ copperEstateAndGold
      actionResult.stacks.deck must_==List()
      there was one(playerOne).newHand(fourCardHand ++ copperEstateAndGold)
    }

    val actionResult = playLibrary(mixedDeckStacks)
    val discardedActions = actionsOf(mixOfAllTypes)

    "discard any action cards dealt in the process" in {
      actionResult.stacks.hand must_==twoCoppers ++ withoutActions(mixOfAllTypes)
      actionResult.stacks.discard must_==discardedActions
    }
    "transmit discard events for those action cards" in {
      checkEventReceived(playerOne, Discard, discardedActions, eventOnlyTable.map(_._2))
    }

    val differentActionResult = playLibrary(evenMoreMixedDeckStacks)
    val firstDiscard = actionsOf(slightlyDifferentMix).take(1)
    val secondDiscard = actionsOf(slightlyDifferentMix).drop(1)

    "should deal as many cards as possible, possibly causing multiple event transmissions" in {
      checkEventReceived(playerOne, Discard, firstDiscard, eventOnlyTable.map(_._2))
      checkEventReceived(playerOne, Discard, secondDiscard, eventOnlyTable.map(_._2))
    }
  }

}