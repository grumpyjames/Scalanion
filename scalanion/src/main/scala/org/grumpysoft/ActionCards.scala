package org.grumpysoft

import actioncards.{Militia, Chancellor}
import org.grumpysoft.TreasureCards.Silver
import collection.immutable.List


object ActionCards {
  object Chapel {
    def apply() : Chapel = { new Chapel }
  }

  def anyEqTo(cards: Seq[Card], candidate: Card): Boolean = {
    !cards.find(b => b.eq(candidate)).isDefined
  }

  class Chapel extends ActionCard(2) {
    def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
      val toTrash = player.chooseFrom(stacks.hand, Trash, 0, 4)
      table.map(_._2.playerEvent(player, Trash, toTrash))
      ActionResult(0, Stacks(stacks.deck, stacks.hand.filter(anyEqTo(toTrash, _)), stacks.discard), supply, table)
    }
    def describe() : String = { "Chapel" }
  }

  object Library {
    def apply() : Library = { new Library }
  }

  def isActionCard(card: Card) : Boolean = card match {
    case ac: ActionCard => true
    case _ => false
  }

  class Library extends ActionCard(2) {
    def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
        ActionResult(0, goUntilSeven(stacks), supply, table)
    }

    def goUntilSeven(stacks: Stacks) : Stacks = {
      if (stacks.hand.length == 7) {
        stacks
      } else {
        val numberToTake: Int = 7 - stacks.hand.length
        val (actions, others) = stacks.deck.take(numberToTake).partition(isActionCard(_))
        goUntilSeven(Stacks(stacks.deck.drop(numberToTake), stacks.hand ++ others, stacks.discard ++ actions))
      }
    }

    def describe() = {
      "Library"
    }
  }

  object Witch {
    def apply() = { new Witch }
  }

  class Witch extends ActionCard(5) {
    def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = { ActionResult(0, Stacks.base(), supply, table) }
    def describe() : String = { "Witch" }
  }


}
