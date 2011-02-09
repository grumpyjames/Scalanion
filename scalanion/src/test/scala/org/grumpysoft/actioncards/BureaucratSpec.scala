package org.grumpysoft.actioncards

import org.grumpysoft.Stacks._
import org.grumpysoft.ActionCards.Bureaucrat
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft.{RevealHand, ActionResult, Stacks, PlaceOnDeck}

object BureaucratSpec extends ActionCardSpecBase {

  val stacks = deckOnly(threeCoppersAndAnEstate)

  val stacksTwo = Stacks(List(Copper()), estateDuchyAndCopper, List())
  val stacksThree = handOnly(twoCoppers)

  val table = makeTable(stacksTwo, stacksThree)


  "bureaucrat" should {

    def playBureaucrat: ActionResult = {
      playerTwo.chooseFrom(estateAndDuchy, PlaceOnDeck, 1, 1) returns estateAndDuchy.take(1)
      Bureaucrat().play(stacks, playerOne, supply, table)
    }

    doBefore {
      supply.available(Silver()) returns true
      supply.buy(Silver()) returns anotherSupply
    }

    "place a silver (from the supply) on top of the player's deck" in {
      val actionResult = Bureaucrat().play(stacks, playerOne, supply, emptyTable)

      there was one(supply).buy(Silver())
      actionResult.supply mustEq(anotherSupply)
      actionResult.stacks must_==deckOnly(List(Silver()) ++ threeCoppersAndAnEstate)
      actionResult.treasure must_==0
    }

    "force each other player to either reveal their hand (containing no victory cards) or place a victory card back on top of their deck" in {
      val actionResult = playBureaucrat
      actionResult.table.head._1 must_==Stacks(Estate() :: stacksTwo.deck, List(Copper(), Duchy()), List())
      actionResult.table.last._1 must_==stacksThree
    }

    "correctly transmit events to other players" in {
      val actionResult = playBureaucrat
      checkEventReceived(playerOne, PlaceOnDeck, List(Silver()), table.map(_._2))
      checkEventReceived(playerTwo, PlaceOnDeck, estateAndDuchy.take(1), List(playerOne, playerThree))
      checkEventReceived(playerThree, RevealHand, twoCoppers, List(playerOne, playerTwo))
    }

  }

}