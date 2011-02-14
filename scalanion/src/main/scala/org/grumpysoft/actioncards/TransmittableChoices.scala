package org.grumpysoft.actioncards

import org.grumpysoft._

trait TransmittableChoices {
  def chooseThenTransmit(player: GenericPlayer[Card], cards: Seq[Card], verb: Verb, minChoices: Int, maxChoices: Int, others: Seq[GenericPlayer[Card]])
    : Seq[Card] = {
    val choices = player.chooseFrom(cards, verb, minChoices, maxChoices)
    others.foreach(_.playerEvent(player, verb, choices))
    return choices
  }
}




