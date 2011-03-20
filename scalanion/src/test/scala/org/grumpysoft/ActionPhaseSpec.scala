package org.grumpysoft

import actioncards.CardFilters
import org.specs.mock.Mockito
import org.specs.Specification
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._

object ActionPhaseSpec extends Specification with Mockito with CardFilters {

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

  val noActionStacks = Stacks.handOnly(List(Copper(), Silver()))

  "the action phase" should {

    "skip itself if there are no action cards in the hand" in {
      val actionResult = runAction(noActionStacks, table)
      there was no(player).chooseFrom(any[Seq[Card]], any[Verb], any[Int], any[Int])
    }

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


