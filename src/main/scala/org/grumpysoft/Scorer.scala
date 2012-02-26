package org.grumpysoft

object Scorer {
  private def scoreStacks(stacks: Stacks): Int = {
    val allCards = stacks.deck ++ stacks.hand ++ stacks.discard
    allCards.map(card => card match {
      case VictoryCard(_, victoryPoints) => victoryPoints
      case c: Curse => -1
      case _ => 0
    }).sum
  }
  
  def leaderboard(table: List[(GenericPlayer[Card], Stacks)]) = {
    table.map(a => (a._1, Scorer.scoreStacks(a._2))).sortBy(-1 * _._2)
  }
}