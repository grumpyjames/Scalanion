package org.grumpysoft

import scala.collection.immutable.Stack

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

import TreasureCards._
import VictoryCards._

class StacksTest extends WordSpec with ShouldMatchers {

  val cards = Stack(Copper(), Copper(), Estate(), Copper(), Estate(), Copper(), Estate())

  "stacks" when {
    "told to end" should {
      "ensure the new hand has five cards in it" in {
	var stacks = Stacks(cards, Stack(), Stack())
	stacks.end.end.hand.size should equal (5) 
      }
    }
  }

}
