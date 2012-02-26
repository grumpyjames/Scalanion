package org.grumpysoft

import org.grumpysoft.TreasureCards._

object GameEndSpec extends org.specs2.mutable.Specification with org.specs2.mock.Mockito {

  case class ThrowingBuyPhaseFn() extends BuyPhaseFn {
    def doBuyPhase(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) : (Supply, Seq[Card]) = {
      throw new RuntimeException("unexpected invocation of action phase: game is over!")
    }
  }

  case class ThrowingActionPhaseFn() extends ActionPhaseFn {
    def doActionPhase(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
      throw new RuntimeException("unexpected invocation of action phase: game is over!")
    }
  }

  trait TestState extends GameSpecState {
    val supply = mock[Supply]
    val postActionSupply = mock[Supply]
    val actionPhase = mock[ActionPhaseFn]
    
    val otherStacks = List.fill[Stacks](3)(deckOnly)
    val playerOneHand = List(Copper(), Silver())
    val allStacks = Stacks(defaultDeck(), playerOneHand, List()) :: otherStacks

    val players = List.fill(4)(new SinkPlayer())

    val table = allStacks.tail.zip(players.tail)

    val gameOverResult = ActionResult.noBuys(1, 2, allStacks.head, postActionSupply, table)
  }

  "a game" should {
    "know when it is over, and not bother with either phase" in new TestState {
      supply.gameOver returns true
      val endOfGame = Game(GameState(players.zip(allStacks), supply), ThrowingBuyPhaseFn(), ThrowingActionPhaseFn()).takeTurn
      endOfGame.isOver must_==true
    }

    "not start the buy phase if the action phase finishes the game" in new TestState {
      supply.gameOver returns false
      postActionSupply.gameOver returns true
      actionPhase.doActionPhase(allStacks.head, players.head, supply, table) returns gameOverResult
      val endOfGame = Game(GameState(players.zip(allStacks), supply), ThrowingBuyPhaseFn(), actionPhase).takeTurn
      endOfGame.isOver must_==true
    }
  }
  
}