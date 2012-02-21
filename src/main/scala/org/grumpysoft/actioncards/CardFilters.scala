package org.grumpysoft.actioncards

import org.grumpysoft.{ActionCard, Card}

trait CardFilters {
  def isActionCard(card: Card) : Boolean = card match {
    case ac: ActionCard => true
    case _ => false
  }
}