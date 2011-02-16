package org.grumpysoft

sealed abstract class Card(cost: Int) extends SelfDescribing {
  def price() : Int = { cost }
  override def toString() : String = { describe() }
}

object ActionResult {
  def noTreasure(buys: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(0, buys, stacks, supply, table)
  }
  def noBuys(treasure: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(treasure, 0, stacks, supply, table)
  }
  def noTreasureOrBuys(stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(0, 0, stacks, supply, table)
  }
}

case class ActionResult(treasure: Int, buys: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) {}


abstract case class ActionCard(cost: Int) extends Card(cost) {
  type Table = Seq[Tuple2[Stacks, GenericPlayer[Card]]]
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult;
}

abstract case class TreasureCard(cost: Int, value: Int) extends Card(cost) {}

abstract case class VictoryCard(cost: Int, victoryPoints: Int) extends Card(cost) {}
