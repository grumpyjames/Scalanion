package org.grumpysoft.actioncards

import org.grumpysoft.Stacks._
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft.{RevealHand, ActionResult, Stacks, PlaceOnDeck}
import org.specs2.specification.Scope

object BureaucratSpec extends ActionCardSpecBase {

  trait State extends ActionCardSpecBase with Scope {
    val stacks = deckOnly(threeCoppersAndAnEstate)

    val stacksTwo = Stacks(List(Copper()), copperDuchyAndEstate, List())
    val stacksThree = handOnly(twoCoppers)

    val table = makeTable(stacksTwo, stacksThree)

    supply.available(Silver()) returns true
    supply.buy(Silver()) returns anotherSupply

    def playBureaucrat: ActionResult = {
      playerTwo.chooseFrom(estateAndDuchy, PlaceOnDeck, 1, 1) returns estateAndDuchy.take(1)
      Bureaucrat().play(stacks, playerOne, supply, table)
    }

  }

  "bureaucrat" should {

    "place a silver (from the supply) on top of the player's deck" in new State {
      val actionResult = Bureaucrat().play(stacks, playerOne, supply, emptyTable)

      there was one(supply).buy(Silver())
      (actionResult.supply must be(anotherSupply)) and
        (actionResult.stacks must_==deckOnly(List(Silver()) ++ threeCoppersAndAnEstate)) and
        (actionResult.treasure must_==0)
    }

    "force each other player to either reveal their hand (containing no victory cards) or place a victory card back on top of their deck" in new State {
      val actionResult = playBureaucrat
      (actionResult.table.head._1 must_==Stacks(Estate() :: stacksTwo.deck, List(Copper(), Duchy()), List())) and
        (actionResult.table.last._1 must_==stacksThree)
    }

    "correctly transmit events to other players" in new State {
      val actionResult = playBureaucrat
      checkEventReceived(playerOne, PlaceOnDeck, List(Silver()), table.map(_._2)) and
        checkEventReceived(playerTwo, PlaceOnDeck, estateAndDuchy.take(1), List(playerOne, playerThree)) and
        checkEventReceived(playerThree, RevealHand, twoCoppers, List(playerOne, playerTwo))
    }

  }

}