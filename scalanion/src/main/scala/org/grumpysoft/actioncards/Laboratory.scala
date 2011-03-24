package org.grumpysoft.actioncards

import org.grumpysoft._

case class Laboratory() extends LaboratoryImpl with PlusCards {
  def count = 2
}

class LaboratoryImpl extends ActionCardImpl {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noTreasureOrBuys(1, stacks, supply, table)
  }

  def describe() = "Laboratory"
  def cost = 5
}




