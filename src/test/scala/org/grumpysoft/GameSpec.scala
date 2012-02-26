package org.grumpysoft

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito

import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft.actioncards.Remodel

object GameSpec extends Specification with Mockito with GameSpecState {

  val supply = mock[Supply]
  val buyPhase = mock[BuyPhaseFn]
  val actionPhase = mock[ActionPhaseFn]

  val players = List.fill(4)(new SinkPlayer())

  val playerOneHand = List(Copper(), Silver())

  val otherStacks = List.fill[Stacks](3)(deckOnly)
  val cannedStacks = Stacks(defaultDeck(), playerOneHand, List()) :: otherStacks
  val postBuySupply = mock[Supply]

  val stacksOne = cannedStacks.head
  val playerOne = players.head
  val table = cannedStacks.tail.zip(players.tail)
  val postActionSupply = mock[Supply]
  val reversedTableResult = ActionResult.noBuys(1, 2, stacksOne, postActionSupply, table.reverse)
  val buyAfterActionResult = (postBuySupply, List(Remodel().toActionCard))

  "a game, when given a meaningful action phase" should {
    supply.gameOver returns false
    actionPhase.doActionPhase(stacksOne, playerOne, supply, table) returns reversedTableResult
    buyPhase.doBuyPhase(1, 1 + 3, playerOne, postActionSupply) returns buyAfterActionResult
    val afterOneTurn = Game(players, cannedStacks, supply, buyPhase, actionPhase).takeTurn

    "correctly delegate to it once per turn" in {
      (there was one(actionPhase).doActionPhase(stacksOne, playerOne, supply, table)) and
        (there was one(buyPhase).doBuyPhase(1, 1 + 3, playerOne, postActionSupply)) and
        (afterOneTurn.allStacks.last.discard must_==Remodel().toActionCard :: stacksOne.hand) and
        (afterOneTurn.supply must_==postBuySupply)
    }
  }

  "a game" should {
    val cardsScoringFive = List(Province(), Curse(), Copper())
    val cardsScoringSix = List(Duchy(), Duchy())
    val cardsScoringTen = List(Province(), Silver(), Gold(), Duchy(), Estate())

    val stacksScoringTwentyOne = Stacks(cardsScoringFive, cardsScoringTen, cardsScoringSix)
    val stacksScoringTwelve = Stacks(cardsScoringSix, Nil, cardsScoringSix)
    val stacksScoringTwentySix = Stacks(cardsScoringTen, cardsScoringSix, cardsScoringTen)

    val associatedPlayers = List.fill(3)(new SinkPlayer)
    val unorderedStacks = List(stacksScoringTwelve, stacksScoringTwentyOne, stacksScoringTwentySix)

    val expectedLeaderboard = associatedPlayers.reverse.zip(List(26, 21, 12))

    "return the leaderboard sorted in order of highest score" in {
      val game = Game(associatedPlayers, unorderedStacks, supply, null, null)
      game.leaderboard must_==expectedLeaderboard
    }
  }

	
}









