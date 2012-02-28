package org.grumpysoft

import TreasureCards._
import VictoryCards._
import annotation.tailrec

object TreasureOrProvincePlayer {
  val description = "buys treasure unless can buy a province"
}

class TreasureOrProvincePlayer extends GenericPlayer[Int] {
  def describe() = TreasureOrProvincePlayer.description

  def gameEvent(event: GameEvent) {}
  def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) {}
  def newHand(hand: Seq[Card]) {}
  def query(question: Query) = { false }

  @tailrec
  private def pickHighestTreasureCardOrProvince(cards: Seq[Card], currentIndex: Int = 1) : Seq[Int] = cards.toList match {
    case VictoryCard(8,6) :: _ => List(currentIndex)
    case TreasureCard(6,3) :: _ => List(currentIndex)
    case TreasureCard(3,2) :: _ => List(currentIndex)
    case _ :: rest => pickHighestTreasureCardOrProvince(rest, currentIndex + 1)
    case Nil => Nil
  }

  def discard(cards: Seq[Card], minChoices: Int) = {
    cards.toList.indices.zip(cards).sortBy(_._2.price()).take(minChoices).map(_._1)
  }

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
    purpose match {
      case Buy => pickHighestTreasureCardOrProvince(cards)
      case Discard => discard(cards, minChoices)
      case _ => List()
    }
  }
}