package org.grumpysoft.actioncards

import collection.immutable.List
import org.grumpysoft._

case class Militia() extends ActionCardImpl with TransmittableChoices {

  type stacksWithPlayer = (Stacks, GenericPlayer[Card])

  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noBuysOrActions(2, stacks, supply, table.map(a => attack(a, player :: allBut(a, table))))
  }

  def attack(underAttack: stacksWithPlayer, others: Seq[GenericPlayer[Card]]) : stacksWithPlayer = {
    val (stacksUnderAttack, playerUnderAttack) = underAttack
    val numberToDiscard = stacksUnderAttack.hand.size - 3
    if (numberToDiscard > 0) {
      val cardsToDiscard = chooseThenTransmit(playerUnderAttack, stacksUnderAttack.hand, Discard, numberToDiscard, numberToDiscard, others)
      return (stacksUnderAttack.discardCards(cardsToDiscard), playerUnderAttack)
    } else {
      return underAttack
    }
  }

  def allBut(victim: stacksWithPlayer, table: Table) : List[GenericPlayer[Card]] = {
    table.filter(_.ne(victim)).map(_._2).toList
  }

  def describe() : String = { "Militia" }

  def cost = 4
}








