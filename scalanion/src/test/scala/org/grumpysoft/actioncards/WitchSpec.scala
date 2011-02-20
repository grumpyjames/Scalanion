package org.grumpysoft.actioncards

import org.grumpysoft.ActionCards.Witch
import org.grumpysoft._
;

object WitchSpec extends ActionCardSpecBase {

  val playerOneStacks = Stacks.deckOnly(copperEstateAndGold)
  val playerTwoStacks = Stacks.empty()
  val playerThreeStacks = Stacks.empty()

  val aCurse = List(Curse())

  "witch" should {
    supply.available(Curse()) returns true
    supply.buy(Curse()) returns anotherSupply
    anotherSupply.buy(Curse()) returns supply
    anotherSupply.available(Curse()) returns true

    val actionResult = Witch().play(playerOneStacks, playerOne, supply, makeTable(playerTwoStacks, playerThreeStacks))
    "curse the other players" in {
      actionResult.table.map(_._1.discard must_==aCurse)
    }
    "transmit events correctly" in {
      checkEventReceived(playerThree, Gain, aCurse, List(playerOne, playerTwo))
      checkEventReceived(playerTwo, Gain, aCurse, List(playerOne, playerThree))
    }
    "add two cards to the player's hand" in {
      actionResult.stacks.hand must_==(copperEstateAndGold.take(2))
    }
    "provide neither treasure nor buys" in {
      actionResult.treasure must_==0
      actionResult.buys must_==0
    }
    "have taken the curses from the supply" in {
      there was one(supply).buy(Curse())
      there was one(anotherSupply).buy(Curse())
      actionResult.supply mustEq(supply)
    }
    case class OneCardSupply(maybeCard: Option[Card]) extends Supply {
      def buy(card: Card) : Supply = {
        OneCardSupply(None)
      }

      def availableCards(treasure: Int) : Seq[Card] = {
        maybeCard match {
          case Some(a) => List(a)
          case None => List.empty[Card]
        }
      }

      def available(card: Card) : Boolean = {
        maybeCard.isDefined && maybeCard.get == card
      }
    }

    "only add curses if they are available in the supply" in {
      val secondActionResult = Witch().play(playerOneStacks, playerOne, OneCardSupply(Some(Curse())), makeTable(playerTwoStacks, playerThreeStacks))
      val resultStacks = secondActionResult.table.map(_._1)
      resultStacks.head.discard must_==aCurse
      resultStacks.last.discard must_==List()
    }
  }

}