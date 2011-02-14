package org.grumpysoft.actioncards

import org.grumpysoft.TreasureCards.Silver
import collection.immutable.List
import org.grumpysoft._

object Bureaucrat {
  def apply() : Bureaucrat = { new Bureaucrat }
}

class Bureaucrat extends ActionCard(4) with TransmittableChoices {
  type stacksWithPlayer = Tuple2[Stacks, GenericPlayer[Card]]
  val oneSilver: List[Silver] = List(Silver())

  def attack(stacksAndPlayer: stacksWithPlayer, otherPlayers: Seq[GenericPlayer[Card]]) : stacksWithPlayer = {
    val (stacks, player) = stacksAndPlayer
    val victoryCards = stacks.hand.filter(isVictoryCard(_))
    if (victoryCards.size > 0) {
      val toReplace = chooseThenTransmit(player, victoryCards, PlaceOnDeck, 1, 1, otherPlayers)
      return (stacks.replace(toReplace), player)
    } else {
      otherPlayers.map(_.playerEvent(player, RevealHand, stacks.hand))
      stacksAndPlayer
    }
  }

  def performAttack(table: Table, player: GenericPlayer[Card]) : Table = {
    table.map(a => attack(a, otherPlayers(player, table, a)))
  }

  def isVictoryCard(card: Card) : Boolean = {
    card match {
      case vc: VictoryCard => true
      case _ => false
    }
  }

  def otherPlayers(player: GenericPlayer[Card], table: Table, a: stacksWithPlayer) : List[GenericPlayer[Card]] = {
    player :: table.filter(_.ne(a)).map(_._2).toList
  }

  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    table.map(_._2.playerEvent(player, PlaceOnDeck, oneSilver))
    ActionResult(0, Stacks(oneSilver ++ stacks.deck, stacks.hand, stacks.discard), supply.buy(Silver()), performAttack(table, player))
  }

  def describe() = { "Bureaucrat" }
}




