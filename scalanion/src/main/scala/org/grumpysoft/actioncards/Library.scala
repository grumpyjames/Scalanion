package org.grumpysoft.actioncards

import org.grumpysoft._

object Library {
  def apply() : Library = { new Library }
}

class Library extends ActionCard(2) {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionResult(0, goUntilSeven(stacks), supply, table)
  }

  def describe() = {
    "Library"
  }

  private def goUntilSeven(stacks: Stacks) : Stacks = {
    if (stacks.hand.length == 7) {
      stacks
    } else {
      val numberToTake: Int = 7 - stacks.hand.length
      val (actions, others) = stacks.deck.take(numberToTake).partition(isActionCard(_))
      goUntilSeven(Stacks(stacks.deck.drop(numberToTake), stacks.hand ++ others, stacks.discard ++ actions))
    }
  }

  private def isActionCard(card: Card) : Boolean = card match {
    case ac: ActionCard => true
    case _ => false
  }
}





