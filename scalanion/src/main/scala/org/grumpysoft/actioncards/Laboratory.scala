package org.grumpysoft.actioncards

import org.grumpysoft._

object Laboratory {
 def apply() = new Laboratory()
}

class Laboratory extends ActionCard(5) {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noTreasureOrBuys(1, stacks.addCards(2), supply, table)
  }

  def describe() = "Laboratory"

  protected def copyThyself() = Laboratory()
}




