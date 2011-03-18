package org.grumpysoft

import TreasureCards._
import VictoryCards._
import actioncards._

object Cards {
  val treasureCards = List(Copper(), Silver(), Gold())
  val actionCards = List(Bureaucrat(), Chancellor(), Chapel(), CouncilRoom(), Library(), Market(), Militia(), Remodel(), Spy(), Thief(), Witch())
  val victoryCards = List(Estate(), Duchy(), Province())
  val cards: List[Card] = treasureCards ++ actionCards ++ victoryCards

  def fromWire(wireFormat: String) : Card = {
    cards.map(_.unapply(wireFormat)).find(_.isDefined).get.get
  }
}