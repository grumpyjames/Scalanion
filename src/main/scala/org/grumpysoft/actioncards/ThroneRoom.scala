package org.grumpysoft.actioncards

import org.grumpysoft._

case class ThroneRoom() extends ActionCardImpl with TransmittableChoices with CardFilters {
  private def toActionCard(card: Card) = card match {
    case ac: ActionCard => ac
    case _ => sys.error("Only call this function with a card you *know* is an action card. Bailing.")
  }

  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val remainingActionCards = stacks.hand.filter(isActionCard(_))
    if (remainingActionCards.isEmpty) return ActionResult.noTreasureOrBuysOrActions(stacks, supply, table)

    val doublePlay = toActionCard(chooseThenTransmit(player, remainingActionCards, Play, 1, 1, table.map(_._2)).head)
    val firstResult = doublePlay.play(stacks.discardCard(doublePlay), player, supply, table)
    doublePlay.play(firstResult.stacks, player, firstResult.supply, firstResult.table).add(firstResult.count)
  }

  def describe() = "ThroneRoom"
  def cost = 4
}




