package org.grumpysoft.actioncards

import org.specs.mock.Mockito
import org.specs.Specification
import org.grumpysoft.{Stacks, GenericPlayer, Card}
import org.grumpysoft.ActionCards._
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft.DiscardYourDeck


object ChancellorSpec extends Specification with Mockito {
  val player = mock[GenericPlayer[Card]]
  val currentDeck = List(Copper(), Copper(), Copper(), Estate())
  val hand = List(Copper(), Copper())
  var discard = List(Remodel())
  val stacks = Stacks(currentDeck, hand, discard)

  "chancellor" should {
    "give player the option to discard their current deck, and do so if they request it" in {
      player.query(DiscardYourDeck) returns true
      val actionResult = Chancellor().play(stacks, player)
      actionResult.treasure must_==2
      actionResult.stacks must_==Stacks(List(), hand, discard ++ currentDeck)
    }

    "just yield +2 treasure if deck is not elected for discard" in {
      player.query(DiscardYourDeck) returns false
      val actionResult = Chancellor().play(stacks, player)
      actionResult.treasure must_==2
      actionResult.stacks mustEq(stacks)
    }

  }
}