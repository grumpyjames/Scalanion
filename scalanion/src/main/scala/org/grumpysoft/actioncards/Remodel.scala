package org.grumpysoft.actioncards

import org.grumpysoft._

object Remodel {
  def apply() : Remodel = {
    new Remodel
  }
}

class Remodel extends ActionCard(4) with TransmittableChoices {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val otherPlayers = table.map(_._2)
    val toRemodel = chooseThenTransmit(player, stacks.hand, Trash, 1, 1, otherPlayers)
    val possibleDestinations = supply.availableCards(toRemodel.head.price + 2)
    val toRemodelTo = chooseThenTransmit(player, possibleDestinations, Gain, 1, 1, otherPlayers)
    ActionResult(0, stacks.trash(toRemodel)._2.gain(toRemodelTo), supply.buy(toRemodelTo.head), table)
  }
  def describe() : String = { "Remodel" }
}