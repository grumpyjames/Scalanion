package org.grumpysoft

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito

import org.grumpysoft.TreasureCards._
import org.grumpysoft.actioncards.Remodel

object GameSpec extends Specification with Mockito with GameSpecState {

  val supply = mock[Supply]
  val buyPhase = mock[BuyPhaseFn]
  val actionPhase = mock[ActionPhaseFn]

  val players = List.fill(4)(new SinkPlayer())

  val playerOneHand = List(Copper(), Silver())

  val otherStacks = List.fill[Stacks](3)(deckOnly)
  val allStacks = Stacks(defaultDeck(), playerOneHand, List()) :: otherStacks
  val postBuySupply = mock[Supply]

  val stacksOne = allStacks.head
  val playerOne = players.head
  val tableWithoutProtagonist = allStacks.tail.zip(players.tail)
  val postActionSupply = mock[Supply]
  val reversedTableResult = ActionResult.noBuys(1, 2, stacksOne, postActionSupply, tableWithoutProtagonist.reverse)
  val buyAfterActionResult = (postBuySupply, List(Remodel().toActionCard))

  "a game, when given a meaningful action phase" should {
    supply.gameOver returns false
    actionPhase.doActionPhase(stacksOne, playerOne, supply, tableWithoutProtagonist) returns reversedTableResult
    buyPhase.doBuyPhase(1, 1 + 3, playerOne, postActionSupply) returns buyAfterActionResult
    val afterOneTurn = Game(GameState(players zip allStacks, supply), buyPhase, actionPhase).takeTurn

    "correctly delegate to it once per turn" in {
      (there was one(actionPhase).doActionPhase(stacksOne, playerOne, supply, tableWithoutProtagonist)) and
        (there was one(buyPhase).doBuyPhase(1, 1 + 3, playerOne, postActionSupply)) and
        (afterOneTurn.state.stacks.last.discard must_==Remodel().toActionCard :: stacksOne.hand) and
        (afterOneTurn.state.supply must_==postBuySupply)
    }
  }

}









