package org.grumpysoft

class SinkPlayer extends GenericPlayer[Card] {
  var hands : List[Seq[Card]] = Nil
  type Event = (SelfDescribing, Verb, Seq[Card])
  var events : List[Event] = Nil

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Card] = {
    List(cards.head)
  }

  def newHand(hand: Seq[Card]) {
    hands = hand :: hands
  }

  def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) {
    events = (player, action, cards) :: events
  }

  def describe() : String = { "test player" }
  def query(question: Query) = true

  def gameEvent(event: GameEvent) { /* just ignore it */ }
}