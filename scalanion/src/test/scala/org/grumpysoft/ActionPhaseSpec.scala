package org.grumpysoft

import org.specs.mock.Mockito
import org.specs.Specification

object ActionPhaseSpec extends Specification with Mockito {

  val actionCard = mock[ActionCard]
  val anotherActionCard = mock[ActionCard]
  val player = mock[GenericPlayer[Card]]
  val supply = mock[Supply]

  val stacks = Stacks(List(), List(), List())

  val actionCards = List(actionCard, anotherActionCard)

  "the action phase" should {

    "offer the player all their remaining actions and then play the selected one" in {
      player.chooseFrom(actionCards, Play, 0, 1) returns List(actionCard)
      ActionPhase(1, stacks, player, actionCards, supply)
      there was one(actionCard).play(stacks, player, supply)
    }

    "be ok with the player choosing not to play an action" in {
      player.chooseFrom(actionCards, Play, 0, 1) returns List()
      ActionPhase(1, stacks, player, actionCards, supply)
      there was one(player).chooseFrom(actionCards, Play, 0, 1)
    }

  }
}

object ActionPhase {

  def apply(actionCount: Int, stacks: Stacks, player: GenericPlayer[Card], actionCards: Seq[ActionCard], supply: Supply) : Unit = {
    val chosen = player.chooseFrom(actionCards, Play, 0, 1)
    chosen.headOption match {
      case Some(a) => a match {
        case b : ActionCard => b.play(stacks, player, supply)
        case _ => throw new IllegalStateException("Something that wasn't an action card was chosen from a set of action cards.")
      }
      case None => doNothing()
    }
  }

  def doNothing() {}

}