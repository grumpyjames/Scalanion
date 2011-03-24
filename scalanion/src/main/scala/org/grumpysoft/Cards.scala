package org.grumpysoft

import TreasureCards._
import VictoryCards._
import actioncards._

object Cards {
  val treasureCards = List(Copper(), Silver(), Gold())
  val actionCards = List(Bureaucrat(), Chancellor(), Chapel(), CouncilRoom(), Library(), Market(), Militia(), Remodel(), Spy(), Thief(), Witch()).map(_.toActionCard)
  val victoryCards = List(Estate(), Duchy(), Province())
  val cards: List[Card] = Curse() :: treasureCards ++ actionCards ++ victoryCards

  def fromWire(wireFormat: String) : Card = {
    val optionCard = cards.map(_.unapply(wireFormat)).find(_.isDefined)
    optionCard match {
      case Some(card) => card.get
      case None => throw new RuntimeException("Card not found: " + wireFormat)
    }
  }
}