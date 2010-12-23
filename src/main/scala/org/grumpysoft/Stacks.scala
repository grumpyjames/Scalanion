package org.grumpysoft

import scala.collection.immutable.Stack
import scala.util.Random._

import TreasureCards.Copper
import VictoryCards.Estate

case class Stacks(deck: Stack[Card], hand: Stack[Card], discard: Stack[Card]) {
  def end() : Stacks = {
    val newHand = deck.take(5)
    if (newHand.size == 5) {
      Stacks(deck.drop(5), newHand, discard ++ hand)
    } else {
      val newDeck = shuffle(discard ++ hand)
      val moreCards = 5 - newHand.size
      val newNewHand = newHand ++ newDeck.take(moreCards)
      Stacks(newDeck.drop(moreCards), newNewHand, Stack())
    }
  }

  override def toString() : String = {
    asString(deck, "deck") + " " + asString(hand, "hand") + " " + asString(discard, "discard")
  }

  private def asString(stack: Stack[Card], name: String) : String = {
    stack.map(_.describe).foldLeft(name + ":")(_ + " " + _)
  }
}

object Stacks {
  def base() : Stacks = {
    val deck = List(Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Estate(), Estate(), Estate())
    Stacks(Stack() ++ shuffle(deck), Stack(), Stack()).end
  }
}
