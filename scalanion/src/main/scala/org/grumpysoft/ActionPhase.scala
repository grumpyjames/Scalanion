package org.grumpysoft

object ActionPhase {

  type Table = Seq[(Stacks, GenericPlayer[Card])]

  private def isActionCard(c: Card) : Boolean = c match {
    case a: ActionCard => true
    case _ => false
  }

  private case class ActionExecution(counts: CountVonCount, stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) {

    def performActions() : ActionResult = counts.actions match {
      case 0 => done
      case _ => playAction()
    }

    private def chosenActionCard = {
      val actionCards = stacks.hand.filter(isActionCard(_))
      player.chooseFrom(actionCards, Play, 0, 1).headOption
    }

    private def done : ActionResult = {
      ActionResult(counts, stacks, supply, table)
    }

    private def playAction() : ActionResult = {
      chosenActionCard match {
        case Some(a) => a match {
          case b : ActionCard => executeAction(b)
          case _ => throw new IllegalStateException("Something that wasn't an action card was chosen from a set of action cards.")
        }
        case None => done
      }
    }

    private def executeAction(actionCard: ActionCard) : ActionResult = {
      val actionResult = oneAction(actionCard)
      ActionPhase(counts + actionResult.count.lessOneAction, actionResult.stacks, player, actionResult.supply, actionResult.table)
    }

    private def oneAction(actionCard: ActionCard) : ActionResult = {
      table.map(_._2.playerEvent(player, Play, List(actionCard)))
      actionCard.play(stacks.discardCard(actionCard), player, supply, table)
    }

  }

  def apply(counts: CountVonCount, stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionExecution(counts, stacks, player, supply, table).performActions
  }

  def doActionPhase(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    ActionExecution(CountVonCount.zero(), stacks, player, supply, table).performActions
  }

}


