package org.grumpysoft

import org.specs.mock.Mockito
import org.specs.Specification
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._

object ActionPhaseSpec extends Specification with Mockito {

  val actionCard = mock[ActionCard]
  val anotherActionCard = mock[ActionCard]
  val player = mock[GenericPlayer[Card]]
  val supply = mock[Supply]

  type Table = Seq[(Stacks, GenericPlayer[Card])]

  val table : Table = List()

  val actionCards = List(actionCard, anotherActionCard)
  val allCards = actionCards ++ List(Copper(), Silver(), Estate())
  

  val stacks = Stacks(List(), allCards, List())
  val stacksPostAction = stacks.discardCard(actionCard)

  "the action phase" should {

    "offer the player all their remaining actions and then play the selected one" in {
      player.chooseFrom(actionCards, Play, 0, 1) returns List(actionCard)
      val actionResult = ActionPhase(1, stacks, player, supply, table)
      there was one(actionCard).play(stacksPostAction, player, supply, table)
    }

    "be ok with the player choosing not to play an action" in {
      player.chooseFrom(actionCards, Play, 0, 1) returns List()
      val actionResult = ActionPhase(1, stacks, player, supply, table)
      there was one(player).chooseFrom(actionCards, Play, 0, 1)
      actionResult.stacks must_==stacks
    }

    "discard the played action card" in {
      player.chooseFrom(actionCards, Play, 0, 1) returns List(actionCard)
      actionCard.play(stacksPostAction, player, supply, table) returns ActionResult.noTreasureOrBuysOrActions(stacksPostAction, supply, table)
      val result = ActionPhase(1, stacks, player, supply, table)
      result.stacks.discard.head must_==actionCard
    }

  }
}

object ActionPhase {

  type Table = Seq[(Stacks, GenericPlayer[Card])]

  def isActionCard(c: Card) : Boolean = c match {
    case a: ActionCard => true
    case _ => false
  }

  private case class OneAction(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) {
    def play(actionCard: ActionCard) : ActionResult = {
      actionCard.play(stacks.discardCard(actionCard), player, supply, table)
    }
  }

  def apply(actionCount: Int, stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val actionCards = stacks.hand.filter(isActionCard(_))
    val chosen = player.chooseFrom(actionCards, Play, 0, 1)
    chosen.headOption match {
      case Some(a) => a match {
        case b : ActionCard => OneAction(stacks, player, supply, table).play(b)
        case _ => throw new IllegalStateException("Something that wasn't an action card was chosen from a set of action cards.")
      }
      case None => ActionResult.noTreasureOrBuysOrActions(stacks, supply, table)
    }
  }

  def doNothing() {}

}
