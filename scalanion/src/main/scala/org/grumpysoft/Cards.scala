package org.grumpysoft

import TreasureCards._
import actioncards._

object Cards {
  val cards: List[Card] =
    List(Copper(), Silver(), Gold(), Bureaucrat(), Chancellor(), Chapel(), CouncilRoom(), Library(), Market(), Militia(), Remodel(), Spy(), Thief(), Witch())

  def fromWire(wireFormat: String) : Card = {
    cards.map(_.unapply(wireFormat)).find(_.isDefined).get.get
  }
}