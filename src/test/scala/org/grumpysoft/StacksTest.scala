package org.grumpysoft

import scala.collection.immutable.Stack

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

import TreasureCards._
import VictoryCards._

class StacksTest extends WordSpec with ShouldMatchers {

  val cards = Stack(Copper(), Copper(), Estate(), Copper(), Estate(), Copper(), Estate())
  val twoCoppers = Stack(Copper(), Copper())

  "stacks" when {
    "told to end" should {
      "ensure the new hand has five cards in it" in {
	var stacks = Stacks(cards, Stack(), Stack())
	stacks.endTurn.endTurn.hand.size should equal (5) 
      }
    }
  }

  "stacks" when {
    "told to add a card" should {
      "ensure that exactly the right number of cards are added, shuffling the discard to recreate the deck if necessary" in {
	var stacks = Stacks(twoCoppers, Stack(), cards)
	stacks.addCards(3).hand.size should equal (3)
      }
    }
  }

  "stacks" when {
    "told to discard a card" should {
      "discard exactly one card of the same type. This card should end up in the discard pile" in {
	var stacks = Stacks(Stack(), cards, Stack())
	stacks.discardCard(cards.top).discard should equal (Stack(cards.top))
      }
    }
  }

  "stacks" when {
    "gaining some cards" should {
      "place them into discard" in {
	var stacks = Stacks(Stack(), Stack(), Stack())
	stacks.gain(List(Copper(), Estate())).discard should equal (Stack(Copper(), Estate()))
      }
    }
  }

  "stacks" when {
    "replacing some cards" should {
      "move them from the hand into the deck" in {
	val stacks = Stacks(Stack(), Stack(Copper(), Estate(), Estate()), Stack())
	val replaced = stacks.replace(List(Estate(), Estate()))
	replaced.deck should equal (Stack(Estate(), Estate()))
	replaced.hand should equal (Stack(Copper()))
      }
    }
  }

  "stacks" when {
    "told to trash a card" should {
      "remove that card from it's hand" in {
	val stacks = Stacks(Stack(), Stack(Copper(), Estate()), Stack())
	val (trashed, newStacks) = stacks.trash(List(Copper()))
	trashed should equal (List(Copper()))
	newStacks.hand should be (Stack(Estate()))
      }
    }
  }
  

}
