package org.grumpysoft.actioncards

import org.grumpysoft._
import org.grumpysoft.VictoryCards._

object SpySpec extends ActionCardSpecBase {

  val playerOneStacks = Stacks.deckOnly(witchAndDuchy)
  val playerTwoStacks = Stacks.deckOnly(copperDuchyAndEstate)
  val playerThreeStacks = Stacks.deckOnly(mixOfAllTypes)

  val table = makeTable(playerTwoStacks, playerThreeStacks)

  val justDuchy = witchAndDuchy.tail


  val justCopper = copperDuchyAndEstate.take(1)
  playerOne.query(ChooseForOtherPlayer(justCopper, playerTwo, Discard)) returns true
  val playerThreeTopCard = mixOfAllTypes.take(1)
  playerOne.query(ChooseForOtherPlayer(playerThreeTopCard, playerThree, Discard)) returns false
  playerOne.chooseFrom(witchAndDuchy.tail, Discard, 0, 1) returns justDuchy

  "spy" should {

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

    "have told the player what their new hand is" in {
      there was one(playerOne).newHand(actionResult.stacks.hand)
    }

    val fourAndFive = List(emptyDeckStacks, oneCardDeckStacks).zip(List(playerFour, playerFive))

    "behave ok when someone who is spied on has a deck of only one card, or no cards at all" in {
      playerOne.query(ChooseForOtherPlayer(twoEstates.take(1), playerFour, Discard)) returns true
      playerOne.query(ChooseForOtherPlayer(oneRemodel, playerFive, Discard)) returns true

      val actionResult = Spy().play(playerOneStacks, playerOne, supply, table ++ fourAndFive)
      val interestingStacks = actionResult.table.drop(2).map(_._1)
      interestingStacks.head.discard must_==List(Estate())
      interestingStacks.last.discard must_==Remodel().toActionCard :: oneCardDeckStacks.discard
    }

  }

}



