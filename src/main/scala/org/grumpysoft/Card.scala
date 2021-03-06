package org.grumpysoft

object Card {
  implicit def toCard(impl: ActionCardImpl) : Card = { ActionCard(impl.cost, impl) }
}

sealed abstract class Card(cost: Int) extends SelfDescribing {
  def price() : Int = { cost }
  override def toString: String = { describe() }
  protected def copyThyself() : Card;

  def unapply(wireFormat: String) : Option[Card] = {
    if (this.describe == wireFormat) Some(this.copyThyself())
    else None
  }

  def times(count: Int) : List[Card] = {
    List.iterate(this, count)(_.copyThyself())
  }
}

object ActionResult {
  def noBuys(treasure: Int, actions: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(CountVonCount(treasure, 0, actions), stacks, supply, table)
  }

  def noTreasure(buys: Int, actions: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(CountVonCount(0, buys, actions), stacks, supply, table)
  }

  def noActions(treasure: Int, buys: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(CountVonCount(treasure, buys, 0), stacks, supply, table)
  }

  def noTreasureOrBuys(actions: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(CountVonCount(0, 0, actions), stacks, supply, table)
  }

  def noTreasureOrActions(buys: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(CountVonCount(0, buys, 0), stacks, supply, table)
  }

  def noBuysOrActions(treasure: Int, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(CountVonCount(treasure, 0, 0), stacks, supply, table)
  }

  def noTreasureOrBuysOrActions(stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) : ActionResult = {
    ActionResult(CountVonCount(0, 0, 0), stacks, supply, table)
  }
}

case class ActionResult(count: CountVonCount, stacks: Stacks, supply: Supply, table: Seq[(Stacks, GenericPlayer[Card])]) {
  // Demeter
  def buys = count.buys
  def actions = count.actions
  def treasure = count.treasure

  def add(otherCount: CountVonCount) : ActionResult = {
    ActionResult(this.count + otherCount, stacks, supply, table)
  }
}

case class CountVonCount(treasure: Int, buys: Int, actions: Int) {
  def +(that: CountVonCount) : CountVonCount = {
    CountVonCount(treasure + that.treasure, buys + that.buys, actions + that.actions)
  }

  def lessOneAction : CountVonCount = {
    CountVonCount(treasure, buys, actions - 1)
  }
}

object CountVonCount {
  def oneAction() = CountVonCount(0,0,1)
  def zero() = CountVonCount(0,0,0)
}

trait ActionCardImpl extends SelfDescribing {
  type StacksWithPlayer = (Stacks, GenericPlayer[Card])
  type Table = Seq[StacksWithPlayer]
  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult;
  def cost() : Int;
  def toActionCard : ActionCard = { ActionCard(cost, this) }
}

case class ActionCard(cost: Int, impl: ActionCardImpl) extends Card(cost) {
  type StacksWithPlayer = (Stacks, GenericPlayer[Card])
  type Table = Seq[StacksWithPlayer]

  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    impl.play(stacks, player, supply, table)
  }

  protected def copyThyself() = { this.copy(cost, impl) }
  def describe() = { impl.describe }
}

abstract case class TreasureCard(cost: Int, value: Int) extends Card(cost) {}

abstract case class VictoryCard(cost: Int, victoryPoints: Int) extends Card(cost) {}

case class Curse() extends Card(0) {
  def describe() = "Curse"
  protected def copyThyself() = Curse()
}
