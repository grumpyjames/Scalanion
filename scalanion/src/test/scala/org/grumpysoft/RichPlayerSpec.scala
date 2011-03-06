package org.grumpysoft

import actioncards.{Witch, Militia}
import org.specs.Specification
import VictoryCards._


object RichPlayerSpec extends Specification {
  class FakePlayer extends GenericPlayer[Int] {
    def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
      purpose must_==Discard
      minChoices must_==2
      maxChoices must_==2
      List(1,2)
    }

    def query(question: Query) = true
    def newHand(hand: Seq[Card]) : Unit = {}
    def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) : Unit = {}
    def describe() : String = {""}
  }

  "when prompted, rich player" should {
    "upcast responses back to rich types" in {
      val richie = new RichPlayer(new FakePlayer)
      val cards = List(Witch(), Militia(), Province())
      richie.chooseFrom(cards, Discard, 2, 2) must_==cards.take(2)
    }
  }

}
