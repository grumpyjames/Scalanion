package org.grumpysoft.actioncards

import org.grumpysoft._

object Market {
  def apply() : Market = { new Market }
}

class Market extends ActionCard(5) {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult(CountVonCount(1, 1, 1), stacks.addCards(1), supply, table)
  }

  def describe() = "Market"
}



