package org.grumpysoft.actioncards

import org.grumpysoft._

object Library {
  def apply() : Library = { new Library }
}

class Library extends ActionCard(2) {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult.noTreasureOrBuys(goUntilSeven(stacks, player, table.map(_._2)), supply, table)
  }

  def describe() = {
    "Library"
  }

  private def goUntilSeven(stacks: Stacks, player: GenericPlayer[Card], otherPlayers: Seq[GenericPlayer[Card]]) : Stacks = {
    if (stacks.hand.length == 7) {
      stacks
    } else {
      val numberToTake: Int = 7 - stacks.hand.length
      val (actions, others) = stacks.deck.take(numberToTake).partition(isActionCard(_))
      otherPlayers.foreach(_.playerEvent(player, Discard, actions))
      goUntilSeven(Stacks(stacks.deck.drop(numberToTake), stacks.hand ++ others, stacks.discard ++ actions), player, otherPlayers)
    }
  }

  private def isActionCard(card: Card) : Boolean = card match {
    case ac: ActionCard => true
    case _ => false
  }
}





