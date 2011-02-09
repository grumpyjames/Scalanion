package org.grumpysoft.actioncards

import org.grumpysoft.ActionCards._
import org.grumpysoft.TreasureCards._
import org.grumpysoft.Trash

import org.specs.mock.Mockito
import org.specs.Specification
import org.grumpysoft.{Card, GenericPlayer, Stacks}

object ChapelSpec extends Specification with Mockito {
  val player = mock[GenericPlayer[Card]]
  val willGetTrashed = List(Copper(), Silver())
  val hand = Copper() :: Remodel() :: willGetTrashed
  val stacks = Stacks(List(), hand, List())

  "chapel" should {
    "offer the player the chance to trash up to four cards from their current hand" in {
      player.chooseFrom(hand, Trash, 0, 4) returns List()
      val newStacks = Chapel().play(stacks, player)
      newStacks must_==(stacks)
    }

    "trash the selected cards" in {
      player.chooseFrom(hand, Trash, 0, 4) returns willGetTrashed
      val newStacks = Chapel().play(stacks, player)
      newStacks.hand must_==List(Copper(), Remodel())
    }
  }

}