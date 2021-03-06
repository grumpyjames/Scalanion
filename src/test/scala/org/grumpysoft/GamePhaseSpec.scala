package org.grumpysoft

import org.grumpysoft.TreasureCards._


object GamePhaseSpec extends org.specs2.mutable.Specification with org.specs2.mock.Mockito {

  trait TestState extends GameSpecState {
    val supply = mockAs[Supply]("initial supply")
    val postBuySupply = mockAs[Supply]("supply after the first buy phase")
    val buyPhase = mock[BuyPhaseFn]

    val buyResult = (postBuySupply, List(Silver()))
    val secondBuyResult = (postBuySupply, Nil)
    val players = List.fill(4)(new SinkPlayer())
    val otherStacks = List.fill[Stacks](3)(deckOnly)
    val playerOneHand = List(Copper(), Silver())
    val allStacks = Stacks(defaultDeck(), playerOneHand, Nil) :: otherStacks

    supply.gameOver returns false
    buyPhase.doBuyPhase(1, 3, players.head, supply) returns buyResult
    buyPhase.doBuyPhase(1, 0, players(1), postBuySupply) returns secondBuyResult

    val game = Game(GameState(players zip allStacks, supply), buyPhase, BoringActionPhase())
    val afterOneTurn = game.takeTurn
    val afterTwoTurns = afterOneTurn.takeTurn
  }

  "a game, when given some stacks" should {

    "end up passing the correct supply and treasure to the buy phase" in new TestState {
      (there was one(buyPhase).doBuyPhase(1, 3, players.head, supply)) and
        (there was one(buyPhase).doBuyPhase(1, 0, players(1), postBuySupply))
    }

    "add the bought cards to the buying player's discard" in new TestState {
      afterOneTurn.state.stacks.last.discard must_==Silver() :: playerOneHand
    }

    "transmit buy events to the other players" in new TestState {
      players.tail.map(player => player.events must_==List((players.head, Gain, List(Silver())))).reduceLeft(_ and _)
    }
  }

}