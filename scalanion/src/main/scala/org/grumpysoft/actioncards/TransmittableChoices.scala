package org.grumpysoft.actioncards

import org.grumpysoft._

trait TransmittableChoices {
  def chooseAndTransmit(player: GenericPlayer[Card], cards: Seq[Card], verb: Verb, minChoices: Int, maxChoices: Int, others: Seq[(Stacks, GenericPlayer[Card])])
    : Seq[Card] = {
    val choices = player.chooseFrom(cards, verb, minChoices, maxChoices)
    others.map(_._2).foreach(_.playerEvent(player, verb, choices))
    return choices
  }
}




