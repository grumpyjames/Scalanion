package org.grumpysoft.actioncards

import org.grumpysoft._

case class Market() extends ActionCardImpl {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val newStacks = stacks.addCards(1)
    player.newHand(newStacks.hand)
    ActionResult(CountVonCount(1, 1, 1), newStacks, supply, table)
  }

  def describe() = "Market"
  def cost = 5
}



