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

  def verifyChoiceAndPlay[T](stacks: Stacks, actionCard: ActionCard, table: Table, result: ActionResult, testBody: () => T) : Unit = {
    player.chooseFrom(stacks.hand.filter(isActionCard(_)), Play, 0, 1) returns List(actionCard)
    actionCard.play(stacks.discardCard(actionCard), player, supply, table) returns result
    testBody()
    there was one(player).chooseFrom(stacks.hand.filter(isActionCard(_)), Play, 0, 1)
  }

  val oneBuyResult: ActionResult = ActionResult.noTreasure(1, 1, anotherTwoActionHandStack, supply, table)

  val threeBuysResult = ActionResult.noTreasureOrActions(3, oneActionHandStacks, supply, table)

  val runAction: () => Unit = () => {
    val result = runAction(twoActionHandStack, table)
    result.buys must_== 4
  }

  val verifySecondChoiceThenPlayAction: () => Unit = () => {
    verifyChoiceAndPlay(anotherTwoActionHandStack, secondActionCard, table, threeBuysResult, runAction)
  }

  val twoTreasureTwoActionResult = ActionResult.noBuys(2, 2, anotherTwoActionHandStack, supply, table)
  val twoTreasureTwoActionResultWithOneActionStacks = ActionResult.noBuys(2, 2, oneActionHandStacks, supply, table)

  val verifyThirdChoiceThenPlay: () => Unit = () => {
    player.chooseFrom(oneActionHandStacks.hand.filter(isActionCard(_)), Play, 0, 1) returns List()
    val actionResult = runAction(twoActionHandStack, table)
    actionResult.treasure must_==4
  }

  val verifySecondAndThirdChoicesThenPlay: () => Unit = () => {
    verifyChoiceAndPlay(anotherTwoActionHandStack, secondActionCard, table, twoTreasureTwoActionResultWithOneActionStacks, verifyThirdChoiceThenPlay)
  }

  def runAction(stacks: Stacks, table: Table) : ActionResult = {
    ActionPhase(CountVonCount.oneAction, stacks, player, supply, table)
  }

  "the action phase" should {

    "offer the player all their remaining actions and then play the selected one" in {
      expectChoiceAndPlay(actionCards, actionCard, table, boringResult)
      val actionResult = runAction(stacks, table)
      there was one(actionCard).play(stacksPostAction, player, supply, table)
    }

    "be ok with the player choosing not to play an action" in {
      player.chooseFrom(actionCards, Play, 0, 1) returns List()
      val actionResult = runAction(stacks, table)
      there was one(player).chooseFrom(actionCards, Play, 0, 1)
      actionResult.stacks must_==stacks
    }

    "discard the played action card" in {
      expectChoiceAndPlay(actionCards, actionCard, table, boringResult)
      val actionResult = runAction(stacks, table)
      actionResult.stacks.discard.head must_==actionCard
    }

    "transmit the played card to the table" in {
      expectChoiceAndPlay(actionCards, actionCard, eventOnlyTable, boringResult)
      val actionResult = runAction(stacks, eventOnlyTable)
      there was one(anotherPlayer).playerEvent(player, Play, List(actionCard))
    }

    "gracefully deals with opportunities to play more than one action" in {
      expectChoiceAndPlay(actionCards, actionCard, table, ActionResult.noTreasureOrBuys(2, stacksPostAction, supply, eventOnlyTable))
      val handAfterFirstAction = actionCards.filter(_.ne(actionCard))
      player.chooseFrom(handAfterFirstAction, Play, 0, 1) returns List()
      val actionResult = runAction(stacks, table)
      there was one(player).chooseFrom(handAfterFirstAction, Play, 0, 1)
    }

    "can play three actions consecutively" in {
      expectChoiceAndPlay(twoActionHandStack, actionCard, table, ActionResult.noTreasureOrBuys(2, anotherTwoActionHandStack, supply, table))
      expectChoiceAndPlay(anotherTwoActionHandStack, secondActionCard, table, remainingActionInHandResult)
      player.chooseFrom(List(thirdActionCard), Play, 0, 1) returns List()
      val actionResult = runAction(twoActionHandStack, table)
      there was one(player).chooseFrom(actionCardsOneAndTwo, Play, 0, 1)
      there was one(player).chooseFrom(actionCardsTwoAndThree, Play, 0, 1)
      there was one(player).chooseFrom(List(thirdActionCard), Play, 0, 1)
    }

    "aggregates buys across multiple action results" in {
      verifyChoiceAndPlay(twoActionHandStack, actionCard, table, oneBuyResult, verifySecondChoiceThenPlayAction)
    }

    "aggregates treasure across multiple action results" in {
      verifyChoiceAndPlay(twoActionHandStack, actionCard, table, twoTreasureTwoActionResult, verifySecondAndThirdChoicesThenPlay)
    }

  }
}

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

}
