package org.grumpysoft.actioncards

import org.grumpysoft._

case class Market() extends MarketImpl with PlusCards {
  def count = 1
}

class MarketImpl extends ActionCardImpl {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult(CountVonCount(1, 1, 1), stacks, supply, table)
  }
  def describe() = "Market"
  def cost = 5
}



