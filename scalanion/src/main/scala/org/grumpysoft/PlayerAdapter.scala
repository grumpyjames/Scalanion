package org.grumpysoft

class PlayerAdapter[U](val adaptTo: GenericPlayer[U], lift: ((Seq[Card],Seq[U]) => Seq[Card])) extends GenericPlayer[Card] {

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Card] = {
    lift(cards, adaptTo.chooseFrom(cards, purpose, minChoices, maxChoices))
  }

  def newHand(hand: Seq[Card]) : Unit = {
    adaptTo.newHand(hand)
  }

  def playerEvent(player: Player, action: Verb, cards: Seq[Card]) : Unit = {
    adaptTo.playerEvent(player, action, cards)
  }

  def describe() : String = {
    adaptTo.describe()
  }

}