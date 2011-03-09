package org.grumpysoft

sealed abstract class Card(cost: Int) extends SelfDescribing {
  def price() : Int = { cost }
  override def toString() : String = { describe() }
  def unapply(wireFormat: String) : Option[Card] = {
    if (this.describe == wireFormat) Some(this)
    else None
  }
}

object ActionResult {
  def noTreasureOrBuys(actions: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(0, 0, actions, stacks, supply, table)
  }

  def noTreasureOrActions(buys: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(0, buys, 0, stacks, supply, table)
  }
  def noBuysOrActions(treasure: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(treasure, 0, 0, stacks, supply, table)
  }
  def noTreasureOrBuysOrActions(stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(0, 0, 0, stacks, supply, table)
  }
}

case class ActionResult(treasure: Int, buys: Int, actions: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) {}


abstract case class ActionCard(cost: Int) extends Card(cost) {
  type StacksWithPlayer = (Stacks, GenericPlayer[Card])
  type Table = Seq[StacksWithPlayer]
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult;
}

abstract case class TreasureCard(cost: Int, value: Int) extends Card(cost) {}

abstract case class VictoryCard(cost: Int, victoryPoints: Int) extends Card(cost) {}

case class Curse() extends Card(0) {
  def describe() = "Curse"
}
