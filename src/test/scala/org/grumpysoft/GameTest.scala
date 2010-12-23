package org.grumpysoft

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class SinkPlayer extends GenericPlayer[Card] {

  var hands : List[Seq[Card]] = List()

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Card] = {
    List(cards.head)
  }

  def newHand(hand: Seq[Card]) : Unit = {
    hands = hand :: hands
  }

  def playerEvent(player: Player, action: Verb, cards: Seq[Card]) : Unit = {

  }

  def describe() : String = { "test player" }
}

class GameTest extends WordSpec with ShouldMatchers {

  def checkStandardHand(player: SinkPlayer) : Unit = {
    player.hands.flatten.groupBy(_.describe).map(a => (a._1, a._2.size)).toList.sortBy(_._2) should equal (List(("Estate", 3),("Copper",7)))  
  }

  "a game" when {
    val player = new SinkPlayer 
    "started with a single player" should {
      "after two turns, updated the player with 7 coppers and 3 estates" in {
	val game = new Game(List(player))
	game.takeTurn.takeTurn
	checkStandardHand(player)
      }
    }
  }

  "a game" when {
    val players = List.fill(4)(new SinkPlayer())
    "given four players" should {
      "after eight turns, have given seven coppers and estates to each" in {
	var game = new Game(players)
	0.until(8).foreach(a => game = game.takeTurn)
	players.foreach(checkStandardHand(_))
      }
    }
  }
	
}









