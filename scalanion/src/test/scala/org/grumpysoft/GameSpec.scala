package org.grumpysoft

import org.specs.Specification
import org.specs.mock.Mockito

import org.grumpysoft.TreasureCards._

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
  }

  val theSupply = mock[Supply]
  val theBuyPhase = mock[BuyPhaseFn]

  def checkStandardHand(player: SinkPlayer) : Unit = {
    player.hands.flatten.groupBy(_.describe).map(a => (a._1, a._2.size)).toList.sortBy(_._2) must_==List(("Estate", 3),("Copper",7))
  }

  case class BoringBuyPhase() extends BuyPhaseFn{
    def doBuyPhase(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) = {
      (supply, Nil)
    }
  }

  def makeGame(players: List[GenericPlayer[Card]]) : Game = {
    Game(players, theSupply, BoringBuyPhase())
  }

  val defaultBuyResult = (theSupply, Nil)

  "a game, when started with a single player" should {
    val player = new SinkPlayer
    val game = makeGame(List(player))
    game.takeTurn
    "after a turn, updated the player with 7 coppers and 3 estates" in {
      checkStandardHand(player)
    }
  }

  val players = List.fill(4)(new SinkPlayer())

  "a game, when given four players" should {
    "after a turn each, have given seven coppers and estates to each" in {
      var game = makeGame(players)
      0.until(4).foreach(a => game = game.takeTurn)
      players.foreach(checkStandardHand(_))
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
    theBuyPhase.doBuyPhase(1, 3, players.head, theSupply) returns buyResult
    theBuyPhase.doBuyPhase(1, 0, players.tail.head, postBuySupply) returns secondBuyResult
    val game = Game(players, cannedStacks, theSupply, theBuyPhase)
    val afterOneTurn = game.takeTurn
    val afterTwoTurns = afterOneTurn.takeTurn

    "end up passing the correct supply and treasure to the buy phase" in {
      there was one(theBuyPhase).doBuyPhase(1, 3, players.head, theSupply)
      there was one(theBuyPhase).doBuyPhase(1, 0, players.tail.head, postBuySupply)
    }

    "add the bought cards to the buying player's discard" in {
      afterOneTurn.allStacks.last.discard must_==Silver() :: playerOneHand
    }

    "transmit buy events to the other players" in {
      players.tail.foreach(player => player.events must_==List((players.head, Gain, List(Silver()))))
    }
  }

	
}









