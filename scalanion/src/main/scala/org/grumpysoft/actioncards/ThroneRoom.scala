package org.grumpysoft.actioncards

import org.grumpysoft._

object ThroneRoom {
 def apply() = new ThroneRoom()
}

class ThroneRoom extends ActionCard(4) with TransmittableChoices with CardFilters {
  private def toActionCard(card: Card) = card match {
    case ac: ActionCard => ac
    case _ => error("ffs")
  }

  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val remainingActionCards = stacks.hand.filter(isActionCard(_))
    if (remainingActionCards.isEmpty) return ActionResult.noTreasureOrBuysOrActions(stacks, supply, table)

    val doublePlay = toActionCard(chooseThenTransmit(player, remainingActionCards, Play, 1, 1, table.map(_._2)).head)
    val firstResult = doublePlay.play(stacks.discardCard(doublePlay), player, supply, table)
    doublePlay.play(firstResult.stacks, player, firstResult.supply, firstResult.table).add(firstResult.count)
  }

  def describe() = "ThroneRoom"

  protected def copyThyself() = ThroneRoom()
}




