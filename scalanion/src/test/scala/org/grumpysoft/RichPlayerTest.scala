package org.grumpysoft

import actioncards.Militia
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

import ActionCards._
import VictoryCards._

class FakePlayer extends GenericPlayer[Int] with ShouldMatchers {
  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
    purpose should be (Discard)
    minChoices should be (2)
    maxChoices should be (2)
    List(1,2)
  }

  def query(question: Query) = true

  def newHand(hand: Seq[Card]) : Unit = {}
  def playerEvent(player: GenericPlayer[Any], action: Verb, cards: Seq[Card]) : Unit = {}
  def describe() : String = {""}
}

class RichPlayerTest extends WordSpec with ShouldMatchers {

  "rich player" when {
    "prompted" should {
      "upcast responses back to rich types" in {
        val richie = new RichPlayer(new FakePlayer)
        val cards = List(Witch(), Militia(), Province())
        richie.chooseFrom(cards, Discard, 2, 2) should equal (cards.take(2))
      }
    }
  }

}
