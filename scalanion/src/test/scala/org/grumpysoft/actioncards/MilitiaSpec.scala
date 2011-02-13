package org.grumpysoft.actioncards

import org.grumpysoft.ActionCards.Militia
import collection.immutable.List
import org.grumpysoft.{Card, Discard, Stacks}

object MilitiaSpec extends ActionCardSpecBase {

  val playerOneStacks = Stacks.empty()
  val sevenCardHandStacks = Stacks(twoCoppers, mixOfAllTypes, List())
  val threeCardHandStacks = Stacks(twoCoppers, estateDuchyAndCopper, List())

  "militia" should {
    val playerTwosDiscardChoices: List[Card] = mixOfAllTypes.take(4)
    playerTwo.chooseFrom(mixOfAllTypes, Discard, 4, 4) returns playerTwosDiscardChoices
    val actionResult = Militia().play(playerOneStacks, playerOne, supply, makeTable(sevenCardHandStacks, threeCardHandStacks))
    "give the player two treasure" in {
      actionResult.treasure must_==2
    }
    "have prompted each player to discard down to a three card hand" in {
      actionResult.table.foreach(a => a._1.hand.size must_==3)
      actionResult.table.head._1.discard must_==playerTwosDiscardChoices
    }
    "have transmitted the correct events to players one and three" in {
      List(playerOne, playerThree).foreach(player =>
        there was one(player).playerEvent(playerTwo, Discard, playerTwosDiscardChoices)
      )
    }
    "leave the player's hand untouched" in {
      actionResult.stacks mustEq(playerOneStacks)
    }
  }

}