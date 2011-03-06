package org.grumpysoft

trait GenericPlayer[+T] extends SelfDescribing {
  /**
   * Choose minChoices <= n <= maxChoices cards from the given sequence, for the given purpose
   * @return the index or indices of the chosen cards.
   */
  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[T];

  /**
   * Ask this player a question
   */
  def query(question: Query) : Boolean;

  /**
   *  Notify player of current hand contents. Called whenever cards
   * are added or removed from the hand due to the actions of other
   * players
   */
  def newHand(hand: Seq[Card]) : Unit;

  /**
   * Notify player of a game event caused by someone (possibly themselves)
   */
  def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) : Unit;
  
}
