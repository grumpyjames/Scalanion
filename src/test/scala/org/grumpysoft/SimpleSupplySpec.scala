package org.grumpysoft

import org.specs.Specification

import TreasureCards._

object SimpleSupplySpec extends Specification {

  val coppers = List(Copper(), Copper())
  val silvers = List(Silver(), Silver())
  val oneGold = List(Gold())

  "simple supply" should {
    "return a new supply with a reduced pile of the card bought" in {
      val supply = SimpleSupply(List(coppers, silvers, oneGold))
      val newSupply = supply.buy(Gold())
      newSupply.availableCards(11) must_==List(Copper(),Silver())
    }
  }
      

}
