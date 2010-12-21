package org.grumpysoft

import scala.collection.immutable.Stack
import scala.util.Random._

import TreasureCards._
import VictoryCards._

case class Hand(val deck: Stack[Card], val discard: Stack[Card], val inHand: Stack[Card]) {
  def this(cards: Seq[Card]) = {
    this(Stack() ++ shuffle(cards), Stack(), Stack())
  }

  def end() : Hand = {
    Hand(deck.drop(5), discard ++ inHand, deck.take(5))
  }
}

object Game {
  def makeHands(count: Int) : List[Hand] = {
    var index = 0
    var handBuild : List[Hand] = List()
    index.until(count).foreach(a => 
      handBuild = new Hand(List(Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Estate(), Estate(), Estate())).end() :: handBuild
    )
    handBuild
  }
}

import Game._

class Game(val players: List[GenericPlayer[Card]], private val hands:List[Hand])  {

  def this(players: List[GenericPlayer[Card]]) = {
    this(players, makeHands(players.size))
  }

  def takeTurn() : Game = {
    val currentHand = hands.head
    val currentPlayer = players.head
    currentPlayer.newHand(currentHand.inHand)
    new Game(players.drop(1) ++ List(currentPlayer), hands.drop(1) ++ List(currentHand.end))
  }

}
