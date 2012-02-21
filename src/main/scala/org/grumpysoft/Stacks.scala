package org.grumpysoft

import scala.util.Random._

import TreasureCards.Copper
import VictoryCards.Estate

case class Stacks(deck: List[Card], hand: List[Card], discard: List[Card]) {

  def endTurn() : Stacks = {
    Stacks(deck, List(), discard ++ hand).addCards(5)
  }

  def recycleDiscard() : Stacks = {
    Stacks(shuffle(discard), hand, Nil)
  }

  def addCards(count: Int) : Stacks = {
    if (deck.size < count) {
      Stacks(Nil, deck ++ hand, discard).recycleDiscard.addCards(count - deck.size)
    }
    else {
      Stacks(deck.drop(count), deck.take(count) ++ hand, discard)
    }
  }

  def gainOne(card: Card) : Stacks = {
    gain(List(card))
  }

  def gain(cards: Seq[Card]) : Stacks = {
    Stacks(deck, hand, cards.toList ++ discard)
  }

  def replace(cards: Seq[Card]) : Stacks = {
    val (deckAdditions, newHand) = hand.partition(card => cards.exists(_.eq(card)))
    Stacks(deckAdditions ++ deck, newHand, discard)
  }

  def trashOne(card: Card) : (Card, Stacks) = {
    val (discarded, newStacks) = trash(List(card))
    (discarded.head, newStacks)
  }

  def trash(cards: Seq[Card]) : (Seq[Card], Stacks) = {
    (hand.intersect(cards), Stacks(deck, hand.diff(cards), discard))
  }

  def discardCard(card: Card) : Stacks = hand.find(_.eq(card)) match {
    case Some(a) => Stacks(deck, hand.filter(_.ne(a)), a :: discard)
    case None => error("argh")
  }

  def discardCards(cards: Seq[Card]) : Stacks = {
    val (newHand, toDiscard) = hand.partition(card => !cards.exists(_.eq(card)))
    Stacks(deck, newHand, toDiscard ++ discard)
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
    deckOnly(shuffle(deck)).endTurn
  }

  def empty() : Stacks = {
    Stacks(List(), List(), List())
  }

  def handOnly(hand: List[Card]) = {
    Stacks(List(), hand, List())
  }

  def deckOnly(deck: List[Card]) = {
    Stacks(deck, List(), List())
  }
}
