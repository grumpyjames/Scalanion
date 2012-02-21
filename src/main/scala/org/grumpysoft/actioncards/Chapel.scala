package org.grumpysoft.actioncards

import org.grumpysoft._

case class Chapel() extends ActionCardImpl with TransmittableChoices {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val toTrash = chooseThenTransmit(player, stacks.hand, Trash, 0, 4, table.map(_._2))
    ActionResult.noTreasureOrBuysOrActions(Stacks(stacks.deck, stacks.hand.filter(anyEqTo(toTrash, _)), stacks.discard), supply, table)
  }
  def describe() : String = { "Chapel" }

  private def anyEqTo(cards: Seq[Card], candidate: Card): Boolean = {
    !cards.find(b => b.eq(candidate)).isDefined
  }

  def cost = 2
}




