package org.grumpysoft.actioncards

import org.grumpysoft._

object Library {
  def apply() : Library = { new Library }
}

class Library extends ActionCard(5) with CardFilters {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noTreasureOrBuysOrActions(goUntilSeven(stacks, player, table.map(_._2)), supply, table)
  }

  def describe() = {
    "Library"
  }

  private def goUntilSeven(stacks: Stacks, player: GenericPlayer[Card], otherPlayers: Seq[GenericPlayer[Card]])
  : Stacks = stacks.hand.length match {
    case 7 => stacks
    case _ => {
      val numberToTake: Int = 7 - stacks.hand.length
      val (actions, others) = stacks.deck.take(numberToTake).partition(isActionCard(_))
      otherPlayers.foreach(_.playerEvent(player, Discard, actions))
      goUntilSeven(Stacks(stacks.deck.drop(numberToTake), stacks.hand ++ others, stacks.discard ++ actions), player, otherPlayers)
    }
  }

  protected def copyThyself() = Library()
}





