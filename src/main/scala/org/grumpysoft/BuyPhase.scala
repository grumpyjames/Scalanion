package org.grumpysoft

import scala.collection.immutable.Stack

trait Supply {
  def availableCards(treasure : Int) : Seq[Card];
  def buy(cards : Card) : Supply;
}

object Trader {
  def valueHand(hand: Seq[Card]) : Int = {
    hand.foldLeft(0)((left, right) => right match {
	case TreasureCard(a, b) => left + b
	case _ => left
    })
  }
}

object BuyPhase {

  def apply(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) : (Supply, Seq[Card]) = {
    trGo(buys, treasure, player, supply, List())
  }


  def trGo(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply, bought: List[Card])
  : (Supply, Seq[Card]) = {
    if (buys == 0) return (supply, bought)
    val choices = player.chooseFrom(supply.availableCards(treasure), Buy, 0, 1)
    val boughtCard = choices.head
    trGo(buys - 1, treasure - boughtCard.price, player, supply.buy(boughtCard), boughtCard :: bought)
  }
}
