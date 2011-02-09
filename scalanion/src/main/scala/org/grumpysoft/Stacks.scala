package org.grumpysoft

import scala.collection.immutable.Stack
import scala.util.Random._

import TreasureCards.Copper
import VictoryCards.Estate

case class Stacks(deck: List[Card], hand: List[Card], discard: List[Card]) {

  def endTurn() : Stacks = {
    Stacks(deck, List(), discard ++ hand).addCards(5)
  }

  def addCards(count: Int) : Stacks = {
    val newHand = deck.take(count) ++ hand
    if (deck.size < count)
      Stacks(shuffle(discard), newHand, List()).addCards(count - deck.size)
    else
      Stacks(deck.drop(count), newHand, discard)
  }

  def gain(cards: Seq[Card]) : Stacks = {
    Stacks(deck, hand, discard ++ cards)
  }

  def replace(cards: Seq[Card]) : Stacks = {
    Stacks(deck ++ hand.intersect(cards), hand.diff(cards), discard)
  }

  def trash(cards: Seq[Card]) : (Seq[Card], Stacks) = {
    (hand.intersect(cards), Stacks(deck, hand.diff(cards), discard))
  }

  def discardCard(card: Card) : Stacks = hand.find(_ == card) match {
    case Some(a) => Stacks(deck, hand.filter(_ != a), discard ++ List(a))
    case None => error("argh")
  }

  override def toString() : String = {
    asString(deck, "deck") + " " + asString(hand, "hand") + " " + asString(discard, "discard")
  }

  private def asString(cards: List[Card], name: String) : String = {
    cards.map(_.describe).foldLeft(name + ":")(_ + " " + _)
  }

}

object Stacks {
  def base() : Stacks = {
    val deck = List(Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Estate(), Estate(), Estate())
    Stacks(List() ++ shuffle(deck), List(), List()).endTurn
  }
}
