package org.grumpysoft.actioncards

import org.grumpysoft._

trait PlusCards extends ActionCardImpl {
  def count : Int;

  abstract override def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val newStacks = stacks.addCards(count)
    player.newHand(newStacks.hand)
    super.play(newStacks, player, supply, table)
  }
}