package org.grumpysoft

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class FakePlayer extends GenericPlayer[Int] with ShouldMatchers {
  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
    purpose should be (Discard)
    minChoices should be (2)
    maxChoices should be (2)
    List(1,2)
  }

  def newHand(hand: Seq[Card]) : Unit = {}
  def playerEvent(player: Player, action: Verb, cards: Seq[Card]) : Unit = {}
  def describe() : String = {""}
}

class RichPlayerTest extends WordSpec with ShouldMatchers {

  "rich player" when {
    "prompted" should {
      "upcast responses back to rich types" in {
	val richie = new RichPlayer(new FakePlayer)
	val cards = List(StringCard("Witch"), StringCard("Militia"), StringCard("Province"))
	richie.chooseFrom(cards, Discard, 2, 2) should be (cards.take(2))
      }
    }
  }

}
