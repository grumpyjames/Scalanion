package org.grumpysoft

import org.specs.Specification

object GameSpec extends Specification {
  class SinkPlayer extends GenericPlayer[Card] {
    var hands : List[Seq[Card]] = List()

    def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Card] = {
      List(cards.head)
    }

    def newHand(hand: Seq[Card]) : Unit = {
      hands = hand :: hands
    }

    def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) : Unit = {}
    def describe() : String = { "test player" }
    def query(question: Query) = true
  }

  def checkStandardHand(player: SinkPlayer) : Unit = {
    player.hands.flatten.groupBy(_.describe).map(a => (a._1, a._2.size)).toList.sortBy(_._2) must_==List(("Estate", 3),("Copper",7))
  }

  "a game, when started with a single player" should {
    val player = new SinkPlayer
    val game = new Game(List(player))
    game.takeTurn.takeTurn
    "after two turns, updated the player with 7 coppers and 3 estates" in {
      checkStandardHand(player)
    }
    "have passed the correct parameters to the buyphase" in {

    }
  }

  "a game, when given four players" should {
    val players = List.fill(4)(new SinkPlayer())
    "after eight turns, have given seven coppers and estates to each" in {
      var game = new Game(players)
      0.until(8).foreach(a => game = game.takeTurn)
      players.foreach(checkStandardHand(_))
    }
  }
	
}









