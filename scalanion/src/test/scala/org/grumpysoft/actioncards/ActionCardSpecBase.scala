package org.grumpysoft.actioncards

import org.specs.mock.Mockito
import org.specs.Specification
import org.grumpysoft.ActionCards._
import org.grumpysoft.TreasureCards._
import org.grumpysoft.VictoryCards._
import org.grumpysoft.{Supply, Card, GenericPlayer}

abstract class ActionCardSpecBase extends Specification with Mockito {

  protected val player = mock[GenericPlayer[Card]]

  protected val threeCoppersAndAnEstate = List(Copper(), Copper(), Copper(), Estate())
  protected val twoCoppers = List(Copper(), Copper())
  protected var oneRemodel = List(Remodel())
  protected val copperAndSilver = List(Copper(), Silver())
  protected val silverRemodelAndTwoCoppers = Copper() :: Remodel() :: copperAndSilver

  val supply = mock[Supply]
  val anotherSupply = mock[Supply]

}