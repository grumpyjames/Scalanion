package org.grumpysoft.actioncards

import org.grumpysoft._

object Remodel {
  def apply() : Remodel = {
    new Remodel
  }
}

class Remodel extends ActionCard(4) with TransmittableChoices {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val toRemodel = chooseAndTransmit(player, stacks.hand, Trash, 1, 1, table)
    val possibleDestinations = supply.availableCards(toRemodel.head.price + 2)
    val toRemodelTo = chooseAndTransmit(player, possibleDestinations, Gain, 1, 1, table)
    ActionResult(0, stacks.trash(toRemodel)._2.gain(toRemodelTo), supply.buy(toRemodelTo.head), table)
  }
  def describe() : String = { "Remodel" }
}
