package org.grumpysoft

trait Player extends SelfDescribing {
  /**
   * Choose minChoices <= n <= maxChoices cards from the given sequence, for the given purpose
   * @return the index or indices of the chosen cards.
   */
  def chooseFrom(cards: Seq[Card], purpose: Verb, maxChoices: Int, minChoices: Int) : Seq[Int];

  /**
   * Notify player of current hand contents. Called whenever cards
   * are added or removed from the hand due to the actions of other
   * players
   */
  def newHand(hand: Seq[Card]) : Unit;

  /**
   * Notify player of a game event caused by someone (possibly themselves)
   */
  def playerEvent(player: Player, action: Verb, cards: Seq[Card]) : Unit;
   
}
   
