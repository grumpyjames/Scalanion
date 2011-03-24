package org.grumpysoft.actioncards

import org.grumpysoft._

case class CouncilRoom() extends CouncilRoomImpl with PlusCards {
  def count = 4
}

class CouncilRoomImpl extends ActionCardImpl {

  private def addCard(a: StacksWithPlayer) : StacksWithPlayer = {
    val newStacks = a._1.addCards(1)
    a._2.newHand(newStacks.hand)
    (newStacks, a._2)
  }

  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noTreasureOrActions(1, stacks, supply, table.map(addCard(_)))
  }

  def describe() = "Council Room"
  def cost = 5
}
