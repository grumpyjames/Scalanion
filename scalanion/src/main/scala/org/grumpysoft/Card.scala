package org.grumpysoft

sealed abstract class Card(cost: Int) extends SelfDescribing {
  def price() : Int = { cost }
  override def toString() : String = { describe() }
}

case class ActionResult(treasure: Int, stacks: Stacks, supply: Supply) {}

abstract case class ActionCard(cost: Int) extends Card(cost) {
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply) : ActionResult;
}

abstract case class TreasureCard(cost: Int, value: Int) extends Card(cost) {}

abstract case class VictoryCard(cost: Int, victoryPoints: Int) extends Card(cost) {}
