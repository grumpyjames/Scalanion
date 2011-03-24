package org.grumpysoft.actioncards

import org.grumpysoft._

case class Market() extends ActionCardImpl {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult(CountVonCount(1, 1, 1), stacks.addCards(1), supply, table)
  }

  def describe() = "Market"
  def cost = 5
}



