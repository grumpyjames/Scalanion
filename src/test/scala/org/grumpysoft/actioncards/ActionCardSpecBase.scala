package org.grumpysoft.actioncards

import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft._

trait ActionCardSpecBase extends Specification with Mockito {

  protected val playerOne = mockAs[GenericPlayer[Card]]("Player One")
  protected val playerTwo = mockAs[GenericPlayer[Card]]("Player Two")
  protected val playerThree = mockAs[GenericPlayer[Card]]("Player Three")
  protected val playerFour = mockAs[GenericPlayer[Card]]("Player Four")
  protected val playerFive = mockAs[GenericPlayer[Card]]("Player Five")

  protected val threeCoppersAndAnEstate = List(Copper(), Copper(), Copper(), Estate())
  protected val twoCoppers = List(Copper(), Copper())
  protected val oneRemodel = List(Remodel().toActionCard)
  protected val copperAndSilver = List(Copper(), Silver())
  protected val silverRemodelAndTwoCoppers = Copper() :: Remodel().toActionCard :: copperAndSilver
  protected val twoEstates = List(Estate(), Estate())
  protected val estateAndDuchy = List(Estate(), Duchy())
  protected val copperDuchyAndEstate = Copper() :: estateAndDuchy
  protected val witchAndDuchy = List(Witch().toActionCard, Duchy())
  protected val copperEstateAndGold = List(Copper(), Estate(), Gold())
  protected val emptyDeckStacks = Stacks(Nil, copperDuchyAndEstate, twoEstates)
  protected val oneCardDeckStacks = Stacks(oneRemodel, twoEstates, Nil)

  protected val mixOfAllTypes = List(Remodel().toActionCard, Copper(), Witch().toActionCard, Gold(), Estate(), Copper(), Copper())

  protected val slightlyDifferentMix = List(Copper(), Remodel().toActionCard, Copper(), Gold(), Estate(), Witch().toActionCard, Copper())

  val fourCardHand = List(Copper(), Copper(), Estate(), Copper())

  protected val supply = mock[Supply]
  protected val anotherSupply = mock[Supply]

  type Table = Seq[(Stacks, GenericPlayer[Card])]

  protected val emptyTable: Table = List()

  protected val eventOnlyTable = makeTable(Stacks.empty(), Stacks.empty())

  /**
   * make a table of player two and player three, giving them the appropriate stacks
   */
  protected def makeTable(stacksTwo: Stacks, stacksThree: Stacks) : Table = {
    List(stacksTwo, stacksThree).zip(List(playerTwo, playerThree))
  }

  protected def makeTable(stacksTwo: Stacks, stacksThree: Stacks, stacksFour: Stacks) : Table = {
    List(stacksTwo, stacksThree, stacksFour).zip(List(playerTwo, playerThree, playerFour))
  }

  def checkEventReceived(player: GenericPlayer[Card], verb: Verb, cards: Seq[Card], players: Seq[GenericPlayer[Card]]) = {
    players.map(otherPlayer => there was one(otherPlayer).playerEvent(player, verb, cards)).reduceLeft(_ and _)
  }

  def actionCardFilter(wantActions : Boolean) : Card => Boolean = {
    { a =>
      a match {
        case b: ActionCard => wantActions
        case _ => !wantActions
      }
    }
  }

  def actionsOf(cards: List[Card]) : List[Card] = {
    cards.filter(actionCardFilter(true))
  }

  def withoutActions(cards: List[Card]) : List[Card] = {
    cards.filter(actionCardFilter(false))
  }

}