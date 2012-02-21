package org.grumpysoft.actioncards

import org.grumpysoft._

object ThiefSpec extends ActionCardSpecBase {

  val stacks = Stacks.empty()

  val playerTwoStacks = Stacks.deckOnly(witchAndDuchy)
  val playerThreeStacks = Stacks.deckOnly(copperAndSilver)
  val playerFourStacks = Stacks.deckOnly(copperDuchyAndEstate)

  val stolenSilver = copperAndSilver.drop(1)

  val table = makeTable(playerTwoStacks, playerThreeStacks, playerFourStacks)
  "thief" should {
    // force player three to trash a silver, then pick it up
    playerOne.chooseFrom(copperAndSilver, ThiefTrash, 1, 1) returns stolenSilver
    playerOne.chooseFrom(stolenSilver, Gain, 0, 1) returns stolenSilver

    // player four must discard a copper, but we don't pick it up
    playerOne.chooseFrom(copperDuchyAndEstate.take(1), Gain, 0, 1) returns List()

    val actionResult = Thief().play(stacks, playerOne, supply, table)
    "add stolen cards to the player's discard" in {
      actionResult.stacks.discard must_==stolenSilver
    }
    "discard any cards revealed that weren't treasure cards" in {
      actionResult.table.head._1.discard must_==witchAndDuchy
      actionResult.table.drop(1).head._1.discard must_==copperAndSilver.take(1)
      actionResult.table.last._1.discard must_==copperDuchyAndEstate.tail.take(1)
    }
    "transmit the discard events correctly" in {
      checkEventReceived(playerOne, Gain, stolenSilver, table.map(_._2))
      checkEventReceived(playerTwo, Discard, witchAndDuchy, List(playerOne ,playerThree, playerFour))
      checkEventReceived(playerThree, Discard, copperAndSilver.take(1), List(playerOne, playerTwo, playerFour))
      checkEventReceived(playerThree, Trash, stolenSilver, List(playerOne, playerTwo, playerFour))
      checkEventReceived(playerFour, Trash, copperDuchyAndEstate.take(1), playerOne :: table.map(_._2).toList.take(2))
      checkEventReceived(playerFour, Discard, copperDuchyAndEstate.tail.take(1), playerOne :: table.map(_._2).toList.take(2))
    }
  }

}

