package org.grumpysoft.acceptance

import org.specs2.Specification
import org.grumpysoft._

import annotation.tailrec
import org.specs2.specification.{Then, Given}
import org.specs2.execute.Result

class SingleGameSpec extends Specification {
  val losingPlayerDescription: String = "buys only treasure"

  def is =
    "Given a two player game"                  ^ game ^
      "Then the player who bought all the provinces should have won"        ^ result ^
  end

  object game extends Given[Seq[(SelfDescribing, Int)]] {
    def extract(text: String) : Seq[(SelfDescribing, Int)] = {
      SingleGame(List(new TreasureOnlyPlayer, new TreasureOrProvincePlayer)
        .map(a => new RichPlayer(a)), Supplies.forTwo())
    }
  }

  object result extends Then[Seq[(SelfDescribing, Int)]] {
    def extract(t: Seq[(SelfDescribing, Int)], text: String) : Result = {
      val scores = t.map(a => (a._1.describe(), a._2))
      scores must_==List((TreasureOrProvincePlayer.description, 51), (losingPlayerDescription, 3))
    }
  }
  class TreasureOnlyPlayer extends GenericPlayer[Int] {
    var result : Seq[(SelfDescribing, Int)] = null
    def describe() = losingPlayerDescription

    def gameEvent(event: GameEvent) {
      event match {
        case End(leaderboard) => {
          result = leaderboard
        }
        case _ => {}
      }
    }
    def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) {}
    def newHand(hand: Seq[Card]) {}
    def query(question: Query) = { false }

    @tailrec
    private def pickHighestTreasureCard(cards: Seq[Card], currentIndex: Int = 1) : Seq[Int] = cards.toList match {
      case TreasureCard(6,3) :: _ => List(currentIndex)
      case TreasureCard(3,2) :: _ => List(currentIndex)
      case _ :: rest => pickHighestTreasureCard(rest, currentIndex + 1)
      case Nil => Nil
    }

    def discard(cards: Seq[Card], minChoices: Int) = {
      cards.toList.indices.zip(cards).sortBy(_._2.price()).take(minChoices).map(_._1)
    }

    def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
      purpose match {
        case Buy => pickHighestTreasureCard(cards)
        case Discard => discard(cards, minChoices)
        case _ => List()
      }
    }
  }
}