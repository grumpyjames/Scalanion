package org.grumpysoft

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

import TreasureCards._
import VictoryCards._

class StacksTest extends WordSpec with ShouldMatchers {

  val cards = List(Copper(), Copper(), Estate(), Copper(), Estate(), Copper(), Estate())
  val twoCoppers = List(Copper(), Copper())

  "stacks" when {
    "told to end" should {
      "ensure the new hand has five cards in it" in {
        var stacks = Stacks(cards, List(), List())
        stacks.endTurn.endTurn.hand.size should equal (5)
      }
    }

    "told to add a card" should {
      "ensure that exactly the right number of cards are added, shuffling the discard to recreate the deck if necessary" in {
        var stacks = Stacks(twoCoppers, List(), cards)
        stacks.addCards(3).hand.size should equal (3)
      }
    }

    "told to discard a card" should {
      "discard exactly one card of the same type. This card should end up in the discard pile" in {
        var stacks = Stacks(List(), cards, List())
        stacks.discardCard(cards.head).discard should equal (List(cards.head))
      }
    }

    "gaining some cards" should {
      "place them into discard" in {
        var stacks = Stacks(List(), List(), List())
        stacks.gain(List(Copper(), Estate())).discard should equal (List(Copper(), Estate()))
      }
    }

    "replacing some cards" should {
      "move them from the hand into the deck" in {
        val stacks = Stacks(List(), List(Copper(), Estate(), Estate()), List())
        val replaced = stacks.replace(List(Estate(), Estate()))
        replaced.deck should equal (List(Estate(), Estate()))
        replaced.hand should equal (List(Copper()))
      }
    }

    "told to trash a card" should {
      "remove that card from it's hand" in {
        val stacks = Stacks(List(), List(Copper(), Estate()), List())
        val (trashed, newStacks) = stacks.trash(List(Copper()))
        trashed should equal (List(Copper()))
        newStacks.hand should equal (List(Estate()))
      }
    }

  }

}
