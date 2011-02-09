package org.grumpysoft

sealed abstract class Card(cost: Int) extends SelfDescribing {
  def price() : Int = { cost }
  override def toString() : String = { describe() }
}

case class ActionResult(val treasure: Int, val stacks: Stacks) {}

abstract case class ActionCard(cost: Int) extends Card(cost) {
  def play(stacks: Stacks, player: GenericPlayer[Card]) : ActionResult;
}

abstract case class TreasureCard(cost: Int, value: Int) extends Card(cost) {}

abstract case class VictoryCard(cost: Int, victoryPoints: Int) extends Card(cost) {}
