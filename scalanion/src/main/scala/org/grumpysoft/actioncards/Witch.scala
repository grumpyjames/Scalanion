package org.grumpysoft.actioncards

import org.grumpysoft._

object Witch {
  def apply() = { new Witch }
}

class Witch extends ActionCard(5) {

  def curseAndTransmit(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, otherPlayers: Seq[GenericPlayer[Card]]) : (Stacks, Supply) = {
    otherPlayers.foreach(_.playerEvent(player, Gain, List(Curse())))
    (stacks.gain(List(Curse())), supply.buy(Curse()))
  }

  def curseOne(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, otherPlayers: Seq[GenericPlayer[Card]]) : (Stacks, Supply) = supply.available(Curse()) match {
    case true => curseAndTransmit(stacks, player, supply, otherPlayers)
    case false => (stacks, supply)
  }

  def curseAll(supply: Supply, witch: GenericPlayer[Card], fullTable: List[(Stacks, GenericPlayer[Card])], table: List[(Stacks, GenericPlayer[Card])])
  : List[(Stacks, Supply)] = table match {
    case toCurse :: _ => {
      val nextResult = curseOne(toCurse._1, toCurse._2, supply, witch :: fullTable.map(_._2).filter(_.ne(toCurse._2)))
      nextResult :: curseAll(nextResult._2, witch, fullTable, table.tail)
    }
    case _ => List.empty[(Stacks, Supply)]
  }

  def play(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val cursedStacksWithSupplies = curseAll(supply, player, table.toList, table.toList)
    ActionResult.noTreasureOrBuys(stacks.addCards(2), cursedStacksWithSupplies.map(_._2).lastOption.getOrElse(supply), cursedStacksWithSupplies.map(_._1).zip(table.map(_._2)))
  }

  def describe() : String = { "Witch" }
}



