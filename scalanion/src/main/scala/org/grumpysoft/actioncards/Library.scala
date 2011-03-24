package org.grumpysoft.actioncards

import org.grumpysoft._

case class Library() extends ActionCardImpl with CardFilters {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val newStacks = goUntilSeven(stacks, player, table.map(_._2))
    player.newHand(newStacks.hand)
    ActionResult.noTreasureOrBuysOrActions(newStacks, supply, table)
  }

  def describe() = {
    "Library"
  }

  def cost = 5

  private def goUntilSeven(stacks: Stacks, player: GenericPlayer[Card], otherPlayers: Seq[GenericPlayer[Card]])
  : Stacks = stacks.hand.size match {
    case 7 => stacks
    case _ => {
      val numberToTake: Int = 7 - stacks.hand.size
      val (actions, others) = stacks.deck.take(numberToTake).partition(isActionCard(_))
      (player :: otherPlayers.toList).foreach(_.playerEvent(player, Discard, actions))
      goUntilSeven(Stacks(stacks.deck.drop(numberToTake), stacks.hand ++ others, stacks.discard ++ actions), player, otherPlayers)
    }
  }
}





