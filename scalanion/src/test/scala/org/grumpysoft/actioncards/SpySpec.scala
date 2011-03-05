package org.grumpysoft.actioncards

import org.grumpysoft._

object SpySpec extends ActionCardSpecBase {

  val playerOneStacks = Stacks.deckOnly(witchAndDuchy)
  val playerTwoStacks = Stacks.deckOnly(copperDuchyAndEstate)
  val playerThreeStacks = Stacks.deckOnly(mixOfAllTypes)

  val table = makeTable(playerTwoStacks, playerThreeStacks)

  val justDuchy = witchAndDuchy.tail

  "spy" should {

    val justCopper = copperDuchyAndEstate.take(1)
    playerOne.query(ChooseForOtherPlayer(justCopper, playerTwo, Discard)) returns true
    val playerThreeTopCard = mixOfAllTypes.take(1)
    playerOne.query(ChooseForOtherPlayer(playerThreeTopCard, playerThree, Discard)) returns false
    playerOne.chooseFrom(witchAndDuchy.tail, Discard, 0, 1) returns justDuchy

    val actionResult = Spy().play(playerOneStacks, playerOne, supply, table)

    "add an action and a card" in {
      actionResult.actions must_==1
      actionResult.stacks.hand must_==witchAndDuchy.take(1)
    }

    "have used player one's choices about everyone's top cards" in {
      actionResult.table.head._1.discard must_==copperDuchyAndEstate.take(1)
      actionResult.table.tail.head._1.deck mustEq(mixOfAllTypes)
      actionResult.stacks.discard must_==justDuchy
    }

    "have transmitted events correctly" in {
      checkEventReceived(playerOne, Discard, justDuchy, List(playerTwo, playerThree))
      checkEventReceived(playerTwo, Discard, justCopper, List(playerOne, playerThree))
      checkEventReceived(playerThree, Reveal, playerThreeTopCard, List(playerOne, playerTwo))
    }

  }

}



