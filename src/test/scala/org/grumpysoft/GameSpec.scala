package org.grumpysoft

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito

import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft.actioncards.Remodel

object GameSpec extends Specification with Mockito {

  class SinkPlayer extends GenericPlayer[Card] {
    var hands : List[Seq[Card]] = Nil
    type Event = (SelfDescribing, Verb, Seq[Card])
    var events : List[Event] = Nil

    def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Card] = {
      List(cards.head)
    }

    def newHand(hand: Seq[Card]) : Unit = {
      hands = hand :: hands
    }

    def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) : Unit = {
      events = (player, action, cards) :: events
    }

    def describe() : String = { "test player" }
    def query(question: Query) = true

    def gameEvent(event: GameEvent) : Unit = {
      /* just ignore it */
    }
  }

  val theSupply = mock[Supply]
  val theBuyPhase = mock[BuyPhaseFn]
  val theActionPhase = mock[ActionPhaseFn]

  def checkStandardHand(player: SinkPlayer) = {
    player.hands.flatten.groupBy(_.describe).map(a => (a._1, a._2.size)).toList.sortBy(_._2) must_==List(("Estate", 3),("Copper",7))
  }

  case class BoringBuyPhase() extends BuyPhaseFn {
    def doBuyPhase(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) = {
      (supply, Nil)
    }
  }

  case class BoringActionPhase() extends ActionPhaseFn {
    def doActionPhase(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
      ActionResult.noTreasureOrBuysOrActions(stacks, supply, table)
    }
  }

  def makeGame(players: List[GenericPlayer[Card]]) : Game = {
    Game(players, theSupply, BoringBuyPhase(), BoringActionPhase())
  }

  val defaultBuyResult = (theSupply, Nil)

  "a game, when started with a single player" should {
    theSupply.gameOver returns false
    val player = new SinkPlayer
    val game = makeGame(List(player))
    game.takeTurn
    "after a turn, updated the player with 7 coppers and 3 estates" in {
      checkStandardHand(player)
    }
  }

  val players = List.fill(4)(new SinkPlayer())

  "a game, when given four players" should {
    theSupply.gameOver returns false
    "after a turn each, have given seven coppers and estates to each" in {
      var game = makeGame(players)
      0.until(4).foreach(a => game = game.takeTurn)
      players.map(checkStandardHand(_)).reduceLeft(_ and _)
    }
  }

  val defaultDeck = Copper().times(10)
  val otherStacks = List.fill[Stacks](3)(Stacks.deckOnly(defaultDeck))

  val playerOneHand = List(Copper(), Silver())
  val cannedStacks = Stacks(defaultDeck, playerOneHand, List()) :: otherStacks
  val postBuySupply = mock[Supply]

  val buyResult = (postBuySupply, List(Silver()))
  val secondBuyResult = (postBuySupply, Nil)

  "a game, when given some canned stacks" should {
    theSupply.gameOver returns false
    theBuyPhase.doBuyPhase(1, 3, players.head, theSupply) returns buyResult
    theBuyPhase.doBuyPhase(1, 0, players.tail.head, postBuySupply) returns secondBuyResult
    val game = Game(players, cannedStacks, theSupply, theBuyPhase, BoringActionPhase())
    val afterOneTurn = game.takeTurn
    val afterTwoTurns = afterOneTurn.takeTurn

    "end up passing the correct supply and treasure to the buy phase" in {
      (there was one(theBuyPhase).doBuyPhase(1, 3, players.head, theSupply)) and
        (there was one(theBuyPhase).doBuyPhase(1, 0, players.tail.head, postBuySupply))
    }

    "add the bought cards to the buying player's discard" in {
      afterOneTurn.allStacks.last.discard must_==Silver() :: playerOneHand
    }

    "transmit buy events to the other players" in {
      players.tail.map(player => player.events must_==List((players.head, Gain, List(Silver())))).reduceLeft(_ and _)
    }
  }

  val stacksOne = cannedStacks.head
  val playerOne = players.head
  val table = cannedStacks.tail.zip(players.tail)
  val postActionSupply = mock[Supply]
  val reversedTableResult = ActionResult.noBuys(1, 2, stacksOne, postActionSupply, table.reverse)
  val buyAfterActionResult = (postBuySupply, List(Remodel().toActionCard))

  "a game, when given a meaningful action phase" should {
    theSupply.gameOver returns false
    theActionPhase.doActionPhase(stacksOne, playerOne, theSupply, table) returns reversedTableResult
    theBuyPhase.doBuyPhase(1, 1 + 3, playerOne, postActionSupply) returns buyAfterActionResult
    val afterOneTurn = Game(players, cannedStacks, theSupply, theBuyPhase, theActionPhase).takeTurn

    "correctly delegate to it once per turn" in {
      (there was one(theActionPhase).doActionPhase(stacksOne, playerOne, theSupply, table)) and
        (there was one(theBuyPhase).doBuyPhase(1, 1 + 3, playerOne, postActionSupply)) and
        (afterOneTurn.allStacks.last.discard must_==Remodel().toActionCard :: stacksOne.hand) and
        (afterOneTurn.supply must_==postBuySupply)
    }
  }

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

  "a game" should {
    "know when it is over, and not bother with either phase" in {
      theSupply.gameOver returns true
      val endOfGame = Game(players, cannedStacks, theSupply, ThrowingBuyPhaseFn(), ThrowingActionPhaseFn()).takeTurn
      endOfGame.isOver must_==true
    }

    "not start the buy phase if the action phase finishes the game" in {
      theSupply.gameOver returns false
      postActionSupply.gameOver returns true
      theActionPhase.doActionPhase(stacksOne, playerOne, theSupply, table) returns reversedTableResult
      val endOfGame = Game(players, cannedStacks, theSupply, ThrowingBuyPhaseFn(), theActionPhase).takeTurn
      endOfGame.isOver must_==true
    }

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
      val game = Game(associatedPlayers, unorderedStacks, theSupply, ThrowingBuyPhaseFn(), ThrowingActionPhaseFn())
      game.leaderboard must_==expectedLeaderboard
    }
  }

	
}









