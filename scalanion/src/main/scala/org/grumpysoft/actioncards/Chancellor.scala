package org.grumpysoft.actioncards

import collection.immutable.List
import org.grumpysoft._

object Chancellor {
 def apply() : Chancellor = { new Chancellor }
}

class Chancellor extends ActionCard(3) {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    player.query(DiscardYourDeck) match {
      case true => {
        table.map(_._2.playerEvent(player, DeckDiscard, stacks.deck))
        ActionResult.noBuysOrActions(2, Stacks(List(), stacks.hand, stacks.discard ++ stacks.deck), supply, table)
      }
      case false => ActionResult.noBuysOrActions(2, stacks, supply, table)
    }
  }

  def describe() : String = {
    "Chancellor"
  }
}









