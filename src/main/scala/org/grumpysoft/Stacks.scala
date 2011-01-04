package org.grumpysoft

import scala.collection.immutable.Stack
import scala.util.Random._

import TreasureCards.Copper
import VictoryCards.Estate

case class Stacks(deck: Stack[Card], hand: Stack[Card], discard: Stack[Card]) {

  def end() : Stacks = {
    Stacks(deck, Stack(), discard ++ hand).addCards(5)
  }

  def addCards(count: Int) : Stacks = {
    val newHand = deck.take(count) ++ hand
    if (deck.size < count)
      Stacks(shuffle(discard), newHand, Stack()).addCards(count - deck.size)
    else
      Stacks(deck.drop(count), newHand, discard)
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
