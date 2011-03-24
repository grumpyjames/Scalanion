package org.grumpysoft.actioncards

import org.grumpysoft._

case class CouncilRoom() extends ActionCardImpl {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noTreasureOrActions(1, stacks.addCards(4), supply, table.map(a => (a._1.addCards(1), a._2)))
  }

  def describe() = "Council Room"
  def cost = 5
}
