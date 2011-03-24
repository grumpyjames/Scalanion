package org.grumpysoft.actioncards

import org.grumpysoft._

case class Laboratory() extends ActionCardImpl {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noTreasureOrBuys(1, stacks.addCards(2), supply, table)
  }

  def describe() = "Laboratory"
  def cost = 3
}




