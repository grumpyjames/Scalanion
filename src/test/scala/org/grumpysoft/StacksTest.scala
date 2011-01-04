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
	stacks.end.end.hand.size should equal (5) 
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
  

}
