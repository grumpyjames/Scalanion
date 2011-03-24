package org.grumpysoft.actioncards

import org.grumpysoft._

case class Remodel() extends ActionCardImpl with TransmittableChoices {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val otherPlayers = table.map(_._2)
    val toRemodel = chooseThenTransmit(player, stacks.hand, Trash, 1, 1, otherPlayers)
    val possibleDestinations = supply.availableCards(toRemodel.head.price + 2)
    val toRemodelTo = chooseThenTransmit(player, possibleDestinations, Gain, 1, 1, otherPlayers)
    ActionResult.noTreasureOrBuysOrActions(stacks.trash(toRemodel)._2.gain(toRemodelTo), supply.buy(toRemodelTo.head), table)
  }
  def describe = "Remodel"
  def cost = 4
}
