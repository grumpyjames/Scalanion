package org.grumpysoft.actioncards

import org.specs.mock.Mockito
import org.specs.Specification
import org.grumpysoft.ActionCards._
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft._

abstract class ActionCardSpecBase extends Specification with Mockito {

  protected val playerOne = mockAs[GenericPlayer[Card]]("Player One")
  protected val playerTwo = mockAs[GenericPlayer[Card]]("Player Two")
  protected val playerThree = mockAs[GenericPlayer[Card]]("Player Three")

  protected val threeCoppersAndAnEstate = List(Copper(), Copper(), Copper(), Estate())
  protected val twoCoppers = List(Copper(), Copper())
  protected val oneRemodel = List(Remodel())
  protected val copperAndSilver = List(Copper(), Silver())
  protected val silverRemodelAndTwoCoppers = Copper() :: Remodel() :: copperAndSilver
  protected val twoEstates = List(Estate(), Estate())
  protected val estateAndDuchy = List(Estate(), Duchy())
  protected val estateDuchyAndCopper = Copper() :: estateAndDuchy
  protected val witchAndDuchy = List(Witch(), Duchy())
  protected val copperEstateAndGold = List(Copper(), Estate(), Gold())

  protected val mixOfAllTypes = List(Remodel(), Copper(), Witch(), Gold(), Estate(), Copper(), Copper())

  val fourCardHand = List(Copper(), Copper(), Estate(), Copper())

  protected val supply = mock[Supply]
  protected val anotherSupply = mock[Supply]

  type Table = Seq[Tuple2[Stacks, GenericPlayer[Card]]]

  protected val emptyTable: Table = List()

  protected val eventOnlyTable = makeTable(Stacks.empty(), Stacks.empty())

  /**
   * make a table of player two and player three, giving them the appropriate stacks
   */
  protected def makeTable(stacksTwo: Stacks, stacksThree: Stacks) : Table = {
    return List(stacksTwo, stacksThree).zip(List(playerTwo, playerThree))
  }

  def checkEventReceived(player: GenericPlayer[Card], verb: Verb, cards: Seq[Card], players: Seq[GenericPlayer[Card]]) : Unit = {
    players.foreach(otherPlayer => there was one(otherPlayer).playerEvent(player, verb, cards))
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