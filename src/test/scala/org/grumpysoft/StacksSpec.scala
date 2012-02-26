package org.grumpysoft

import TreasureCards._
import VictoryCards._
import org.specs2.mutable.Specification

class StacksSpec extends Specification {

  val cards = List(Copper(), Copper(), Estate(), Copper(), Estate(), Copper(), Estate())
  val twoCoppers = List(Copper(), Copper())

  "stacks" should {
    "ensure new hands has five cards in them" in {
      val stacks = Stacks(cards, List(), List())
      stacks.endTurn().endTurn().hand.size must_==(5)
    }

    "ensure that exactly the right number of cards are added, shuffling the discard to recreate the deck if necessary" in {
      val stacks = Stacks(twoCoppers, List(), cards)
      stacks.addCards(3).hand.size must_==(3)
    }

    "discard exactly one card of the same type. This card should end up in the discard pile" in {
      val stacks = Stacks(List(), cards, List())
      stacks.discardCard(cards.head).discard must_==(List(cards.head))
    }

    "gaining cards into the discard pile" in {
      val stacks = Stacks(List(), List(), List())
      stacks.gain(List(Copper(), Estate())).discard must_==(List(Copper(), Estate()))
    }

    "replace cards by moving them from the hand into the deck" in {
      val currentHand: List[Card] = List(Copper(), Estate(), Estate())
      val stacks = Stacks(List(), currentHand, List())
      val replaced = stacks.replace(currentHand.init)
      replaced.deck must_==(List(Copper(), Estate()))
      replaced.hand must_==(List(Estate()))
    }

    "trash cards by removing them from hand" in {
        val stacks = Stacks(List(), List(Copper(), Estate()), List())
        val (trashed, newStacks) = stacks.trash(List(Copper()))
        (trashed must_==(List(Copper()))) and
          (newStacks.hand must_==(List(Estate())))
    }
  }
}
