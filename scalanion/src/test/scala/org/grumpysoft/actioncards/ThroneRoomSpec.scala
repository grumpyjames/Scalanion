package org.grumpysoft.actioncards

import org.grumpysoft._

object ThroneRoomSpec extends ActionCardSpecBase {

  val playTwiceAction = mockAs[ActionCard]("the action to play twice")

  val playerOneStacks = Stacks.handOnly(playTwiceAction :: mixOfAllTypes)

  def actionCardsOf(cards: List[Card]) : List[Card] = {
    cards.filter(a => a match {
      case ac: ActionCard => true
      case _ => false
    })
  }

  def playThroneRoom : ActionResult = {
    playerOne.chooseFrom(actionCardsOf(playerOneStacks.hand), Play, 1, 1) returns List(playTwiceAction)
    playTwiceAction.play(playerOneStacks.discardCard(playTwiceAction), playerOne, supply, eventOnlyTable) returns ActionResult.noActions(1, 2, playerOneStacks, anotherSupply, eventOnlyTable)
    playTwiceAction.play(playerOneStacks, playerOne, anotherSupply, eventOnlyTable) returns ActionResult.noActions(2, 5, playerOneStacks, anotherSupply, eventOnlyTable)
    ThroneRoom().play(playerOneStacks, playerOne, supply, eventOnlyTable)
  }

  "throne room" should {
    "play the selected action twice" in {
      val actionResult = playThroneRoom
      there was one(playTwiceAction).play(playerOneStacks, playerOne, anotherSupply, eventOnlyTable)
    }

    "sum the results of the selected action" in {
      val actionResult = playThroneRoom
      actionResult.treasure must_==3
      actionResult.buys must_==7
    }

    "not prompt for a further action if there are none in the hand" in {
      val actionResult = ThroneRoom().play(Stacks.handOnly(copperDuchyAndEstate), playerOne, supply, eventOnlyTable)
      actionResult.buys must_==0
      actionResult.treasure must_==0
      actionResult.actions must_==0
    }

    "transmits the play event for both action plays" in {
      val actionResult = playThroneRoom
      checkEventReceived(playerOne, Play, List(playTwiceAction), eventOnlyTable.map(_._2))
    }
  }

}