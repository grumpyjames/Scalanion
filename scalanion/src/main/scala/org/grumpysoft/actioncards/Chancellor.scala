package org.grumpysoft.actioncards

import collection.immutable.List
import org.grumpysoft._

case class Chancellor() extends ActionCardImpl {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    player.query(BasicQuestion("do you want to discard your deck?")) match {
      case true => {
        table.map(_._2.playerEvent(player, DeckDiscard, stacks.deck))
        ActionResult.noBuysOrActions(2, Stacks(List(), stacks.hand, stacks.discard ++ stacks.deck), supply, table)
      }
      case false => ActionResult.noBuysOrActions(2, stacks, supply, table)
    }
  }

  def cost = 3

  def describe() : String = {
    "Chancellor"
  }
}









