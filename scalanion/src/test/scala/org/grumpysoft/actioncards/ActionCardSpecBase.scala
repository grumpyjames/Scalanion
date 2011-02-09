package org.grumpysoft.actioncards

import org.specs.mock.Mockito
import org.specs.Specification
import org.grumpysoft.ActionCards._
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft._

abstract class ActionCardSpecBase extends Specification with Mockito {

  protected val playerOne = mock[GenericPlayer[Card]]
  protected val playerTwo = mock[GenericPlayer[Card]]
  protected val playerThree = mock[GenericPlayer[Card]]

  protected val threeCoppersAndAnEstate = List(Copper(), Copper(), Copper(), Estate())
  protected val twoCoppers = List(Copper(), Copper())
  protected var oneRemodel = List(Remodel())
  protected val copperAndSilver = List(Copper(), Silver())
  protected val silverRemodelAndTwoCoppers = Copper() :: Remodel() :: copperAndSilver
  protected val twoEstates = List(Estate(), Estate())
  protected val estateAndDuchy = List(Estate(), Duchy())
  protected val estateDuchyAndCopper = Copper() :: estateAndDuchy

  protected val supply = mock[Supply]
  protected val anotherSupply = mock[Supply]

  type Table = Sequence[Tuple2[Stacks, GenericPlayer[Card]]]

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

}