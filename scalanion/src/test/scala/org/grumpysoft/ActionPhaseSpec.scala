package org.grumpysoft

import org.specs.mock.Mockito
import org.specs.Specification
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._

object ActionPhaseSpec extends Specification with Mockito {

  val actionCard = mockAs[ActionCard]("action card one")
  val anotherActionCard = mockAs[ActionCard]("another action card")
  val secondActionCard = mockAs[ActionCard]("action card two")
  val thirdActionCard = mockAs[ActionCard]("action card three")
  val player = mock[GenericPlayer[Card]]
  val anotherPlayer = mock[GenericPlayer[Card]]
  val supply = mock[Supply]

  type Table = Seq[(Stacks, GenericPlayer[Card])]

  val table : Table = List()
  val eventOnlyTable  = List((Stacks.empty, anotherPlayer)) 

  val actionCards = List(actionCard, anotherActionCard)
  val allCards = actionCards ++ List(Copper(), Silver(), Estate())

  val stacks = Stacks.handOnly(allCards)
  val stacksPostAction = stacks.discardCard(actionCard)

  val actionCardsOneAndTwo = List(actionCard, secondActionCard)
  val twoActionHandStack = Stacks.handOnly(actionCardsOneAndTwo)

  val actionCardsTwoAndThree = List(secondActionCard, thirdActionCard)
  val anotherTwoActionHandStack =  Stacks.handOnly(actionCardsTwoAndThree)
  val oneActionHandStacks = Stacks.handOnly(List(thirdActionCard))

  val boringResult = ActionResult.noTreasureOrBuysOrActions(stacksPostAction, supply, table)

  val remainingActionInHandResult = ActionResult.noTreasureOrBuysOrActions(oneActionHandStacks, supply, table)

  def isActionCard(c: Card) : Boolean = c match {
    case ActionCard(_) => true
    case _ => false
  }

  def expectChoiceAndPlay(actionCards: Seq[Card], actionCard: ActionCard, table: Table, result: ActionResult) : Unit = {
    player.chooseFrom(actionCards, Play, 0, 1) returns List(actionCard)
    actionCard.play(stacksPostAction, player, supply, table) returns result
  }

  def expectChoiceAndPlay(stacks: Stacks, actionCard: ActionCard, table: Table, result: ActionResult) : Unit = {
    player.chooseFrom(stacks.hand.filter(isActionCard(_)), Play, 0, 1) returns List(actionCard)
    actionCard.play(stacks.discardCard(actionCard), player, supply, table) returns result
  }

  "the action phase" should {

    "offer the player all their remaining actions and then play the selected one" in {
      expectChoiceAndPlay(actionCards, actionCard, table, boringResult)
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
      expectChoiceAndPlay(actionCards, actionCard, table, boringResult)
      val result = ActionPhase(1, stacks, player, supply, table)
      result.stacks.discard.head must_==actionCard
    }

    "transmit the played card to the table" in {
      expectChoiceAndPlay(actionCards, actionCard, eventOnlyTable, boringResult)
      val result = ActionPhase(1, stacks, player, supply, eventOnlyTable)
      there was one(anotherPlayer).playerEvent(player, Play, List(actionCard))
    }

    "gracefully deals with opportunities to play more than one action" in {
      expectChoiceAndPlay(actionCards, actionCard, table, ActionResult.noTreasureOrBuys(2, stacksPostAction, supply, eventOnlyTable))
      val handAfterFirstAction = actionCards.filter(_.ne(actionCard))
      player.chooseFrom(handAfterFirstAction, Play, 0, 1) returns List()
      val result = ActionPhase(1, stacks, player, supply, table)
      there was one(player).chooseFrom(handAfterFirstAction, Play, 0, 1)
    }

    "can play three actions consecutively" in {
      expectChoiceAndPlay(twoActionHandStack, actionCard, table, ActionResult.noTreasureOrBuys(2, anotherTwoActionHandStack, supply, table))
      expectChoiceAndPlay(anotherTwoActionHandStack, secondActionCard, table, remainingActionInHandResult)
      player.chooseFrom(List(thirdActionCard), Play, 0, 1) returns List()
      val result = ActionPhase(1, twoActionHandStack, player, supply, table)
      there was one(player).chooseFrom(actionCardsOneAndTwo, Play, 0, 1)
      there was one(player).chooseFrom(actionCardsTwoAndThree, Play, 0, 1)
      there was one(player).chooseFrom(List(thirdActionCard), Play, 0, 1)
    }

    "aggregates buys across multiple action results" in {

    }

    "aggregates treasure across multiple action results" in {

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
      table.map(_._2.playerEvent(player, Play, List(actionCard)))
      actionCard.play(stacks.discardCard(actionCard), player, supply, table)
    }
  }

  def apply(actionCount: Int, stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
    val actionCards = stacks.hand.filter(isActionCard(_))
    val chosen = player.chooseFrom(actionCards, Play, 0, 1)
    chosen.headOption match {
      case Some(a) => a match {
        case b : ActionCard => {
          val actionResult = OneAction(stacks, player, supply, table).play(b)
          val remainingActions = actionResult.actions + actionCount - 1
          if (remainingActions > 0) ActionPhase(remainingActions, actionResult.stacks, player, actionResult.supply, actionResult.table)
          else actionResult
        }
        case _ => throw new IllegalStateException("Something that wasn't an action card was chosen from a set of action cards.")
      }
      case None => ActionResult.noTreasureOrBuysOrActions(stacks, supply, table)
    }
  }

  def doNothing() {}

}
