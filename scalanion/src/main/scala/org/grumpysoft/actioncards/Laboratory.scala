package org.grumpysoft.actioncards

import org.grumpysoft._

case class Laboratory() extends ActionCardImpl {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val newStacks = stacks.addCards(2)
    player.newHand(newStacks.hand)
    ActionResult.noTreasureOrBuys(1, newStacks, supply, table)
  }

  def describe() = "Laboratory"
  def cost = 5
}




