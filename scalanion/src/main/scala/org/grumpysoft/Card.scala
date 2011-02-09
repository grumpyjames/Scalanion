package org.grumpysoft

sealed abstract class Card(cost: Int) extends SelfDescribing {
  def price() : Int = { cost }
  override def toString() : String = { describe() }
}

case class ActionResult(treasure: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) {}


abstract case class ActionCard(cost: Int) extends Card(cost) {
  type Table = Sequence[Tuple2[Stacks, GenericPlayer[Card]]]
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult;
}

abstract case class TreasureCard(cost: Int, value: Int) extends Card(cost) {}

abstract case class VictoryCard(cost: Int, victoryPoints: Int) extends Card(cost) {}
