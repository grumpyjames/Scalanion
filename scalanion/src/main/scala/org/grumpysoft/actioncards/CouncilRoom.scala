package org.grumpysoft.actioncards

import org.grumpysoft._

object CouncilRoom {
  def apply() : CouncilRoom = { new CouncilRoom }
}

class CouncilRoom extends ActionCard(5) {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noTreasure(1, stacks.addCards(4), supply, table.map(a => (a._1.addCards(1), a._2)))
  }

  def describe() = "Council Room"
}
