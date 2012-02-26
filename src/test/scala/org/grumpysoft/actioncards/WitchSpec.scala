package org.grumpysoft.actioncards

import org.grumpysoft._

object WitchSpec extends ActionCardSpecBase {

  trait TestState extends ActionCardSpecBase with org.specs2.specification.Scope {
    val playerOneStacks = Stacks.deckOnly(copperEstateAndGold)
    val playerTwoStacks = Stacks.empty()
    val playerThreeStacks = Stacks.empty()

    val aCurse = List(Curse())

    supply.available(Curse()) returns true
    supply.buy(Curse()) returns anotherSupply
    anotherSupply.buy(Curse()) returns supply
    anotherSupply.available(Curse()) returns true

    val actionResult = Witch().play(playerOneStacks, playerOne, supply, makeTable(playerTwoStacks, playerThreeStacks))

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

      def gameOver() = false
    }
  }
  

  "witch" should {
    "curse the other players" in new TestState {
      actionResult.table.map(_._1.discard must_==aCurse)
    }

    "transmit events correctly" in new TestState {
      checkEventReceived(playerThree, Gain, aCurse, List(playerOne, playerTwo))
      checkEventReceived(playerTwo, Gain, aCurse, List(playerOne, playerThree))
    }

    "add two cards to the player's hand" in new TestState {
      actionResult.stacks.hand must_==(copperEstateAndGold.take(2))
    }

    "provide neither treasure nor buys" in new TestState {
      actionResult.treasure must_==0
      actionResult.buys must_==0
    }

    "have taken the curses from the supply" in new TestState {
      there was one(supply).buy(Curse())
      there was one(anotherSupply).buy(Curse())
      actionResult.supply mustEqual(supply)
    }


    "only add curses if they are available in the supply" in new TestState {
      val secondActionResult = Witch().play(playerOneStacks, playerOne, OneCardSupply(Some(Curse())), makeTable(playerTwoStacks, playerThreeStacks))
      val resultStacks = secondActionResult.table.map(_._1)
      resultStacks.head.discard must_==aCurse
      resultStacks.last.discard must_==List()
    }
  }

}