package org.grumpysoft

trait Player {
  /**
   * Choose minChoices <= n <= maxChoices cards from the given sequence, for the given purpose
   * @return the index or indices of the chosen cards.
   */
  def chooseFrom(cards: Seq[Card], purpose: Verb, maxChoices: Int, minChoices: Int) : Seq[Int];
}
