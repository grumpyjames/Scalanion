package org.grumpysoft

import annotation.tailrec

object Trader {
  def valueHand(hand: Seq[Card]) : Int = {
    hand.foldLeft(0)((left, right) => right match {
	    case TreasureCard(a, b) => left + b
	    case _ => left
    })
  }
}

object Score {
  def scoreStacks(stacks: Stacks) : Int = {
    val allCards = stacks.deck ++ stacks.hand ++ stacks.discard
    allCards.map(card => card match {
      case VictoryCard(_, victoryPoints) => victoryPoints
      case c : Curse => -1
      case _ => 0
    }).sum
  }
}

object BuyPhase {
  def apply(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) : (Supply, Seq[Card]) = {
    trGo(buys, treasure, player, supply, List())
  }

  @tailrec
  def trGo(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply, bought: List[Card])
  : (Supply, Seq[Card]) = {
    if (buys == 0) return (supply, bought)

    val choices = player.chooseFrom(supply.availableCards(treasure), Buy, 0, 1)
    if (choices.size == 0)
      return (supply, bought)
    
    val boughtCard = choices.head
    return trGo(buys - 1, treasure - boughtCard.price, player, supply.buy(boughtCard), boughtCard :: bought)
  }
}
