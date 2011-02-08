package org.grumpysoft

import org.specs.Specification
import org.specs.mock.Mockito
import org.mockito.Matchers._

import scala.collection.immutable.Stack

import TreasureCards._
import VictoryCards._

object BuyPhaseSpec extends Specification with Mockito {

  val deck = Stack(Copper(), Copper(), Copper(), Estate(), Estate())
  val hand = Stack(Copper(), Copper(), Copper(), Copper(), Estate())
  val actionlessStacks = Stacks(deck, hand, Stack())

  val player = mock[GenericPlayer[Card]]
  val supply = mock[Supply]

  val copperAndSilver = List(Copper(), Silver())
  val justCopper = List(Copper())
  val justSilver = List(Silver())

  "the buy phase" should {
    "immediately offer the actionless player buy choices from the supply" in {
      supply.availableCards(4) returns copperAndSilver
      player.chooseFrom(copperAndSilver, Buy, 0, 1) returns justCopper
      supply.buy(Copper()) returns supply
      val (newSupply, bought) = BuyPhase(1, 4, player, supply)
      bought must_==List(Copper())
    }
  }

  val copperSupply = mock[Supply]
  val emptySupply = mock[Supply]

  "the buy phase" should {
    "offer the player a buy for each one available" in {
      supply.availableCards(4) returns copperAndSilver
      supply.buy(Silver()) returns copperSupply
      copperSupply.availableCards(1) returns justCopper
      copperSupply.buy(Copper()) returns emptySupply
      player.chooseFrom(copperAndSilver, Buy, 0, 1) returns justSilver
      player.chooseFrom(justCopper, Buy, 0, 1) returns justCopper
      val (newSupply, bought) = BuyPhase(2, 4, player, supply)
      bought must_==List(Copper(), Silver())
    }
  }

  "the buy phase" should {
    "be ok with absolutely no buys" in {
      supply.availableCards(4) returns justCopper
      player.chooseFrom(justCopper, Buy, 0, 1) returns List()
      val (newSupply, bought) = BuyPhase(1, 4, player, supply)
      bought must_==List()
    }
  }
}
