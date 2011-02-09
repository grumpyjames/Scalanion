package org.grumpysoft.actioncards

import org.grumpysoft.Stacks._
import org.grumpysoft.ActionCards.Bureaucrat
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft.{Stacks, PlaceOnDeck}

object BureaucratSpec extends ActionCardSpecBase {

  val stacks = deckOnly(threeCoppersAndAnEstate)

  val stacksTwo = Stacks(List(Copper()), estateDuchyAndCopper, List())
  val stacksThree = handOnly(twoCoppers)

  val table = makeTable(stacksTwo, stacksThree)

  "bureaucrat" should {

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
      playerTwo.chooseFrom(estateAndDuchy, PlaceOnDeck, 1, 1) returns estateAndDuchy.take(1)
      val actionResult = Bureaucrat().play(stacks, playerOne, supply, table)
      actionResult.table.head._1 must_==Stacks(Estate() :: stacksTwo.deck, List(Copper(), Duchy()), List())
      actionResult.table.last._1 must_==stacksThree
    }

  }

}