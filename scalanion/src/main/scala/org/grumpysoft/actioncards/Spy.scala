package org.grumpysoft.actioncards

import org.grumpysoft._

object Spy {
  def apply() : Spy = { new Spy }
}

class Spy extends ActionCard(4) with TransmittableChoices {
  def describe() = "Spy"

  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val newTable = table.map(a => spyOn(player, table.map(_._2).filter(_.ne(a._2)).toList, a))
    val plusOneCard = stacks.addCards(1)
    chooseThenTransmit(player, plusOneCard.deck.take(1), Discard, 0, 1, table.map(_._2)).size > 0 match {
      case false => ActionResult.noTreasureOrBuys(1, plusOneCard, supply, newTable)
      case true => ActionResult.noTreasureOrBuys(1, Stacks(plusOneCard.deck.tail, plusOneCard.hand, plusOneCard.deck.head :: plusOneCard.discard), supply, newTable)
    }
  }

  private def spyOn(spy: GenericPlayer[Card], others: List[GenericPlayer[Card]], stacksWithPlayer: StacksWithPlayer) : StacksWithPlayer = {
    val topCard = stacksWithPlayer._1.deck.take(1)
    spy.query(ChooseForOtherPlayer(topCard, stacksWithPlayer._2, Discard)) match {
      case true => doDiscard(stacksWithPlayer, spy :: others)
      case false => doReveal(stacksWithPlayer, spy :: others)
    }
  }

  private def doDiscard(spiedOn: StacksWithPlayer, otherPlayers: List[GenericPlayer[Card]]) : StacksWithPlayer = {
    val (stacks, victim) = spiedOn
    otherPlayers.map(_.playerEvent(victim, Discard, stacks.deck.take(1)))
    (Stacks(stacks.deck.tail, stacks.hand, stacks.deck.head :: stacks.discard), victim)
  }

  private def doReveal(spiedOn: StacksWithPlayer, otherPlayers: List[GenericPlayer[Card]]) : StacksWithPlayer = {
    val (stacks, victim) = spiedOn
    otherPlayers.map(_.playerEvent(victim, Reveal, stacks.deck.take(1)))
    (stacks, victim)
  }

}



