package org.grumpysoft

import actioncards.{Remodel, Witch}
import org.specs.Specification
import org.grumpysoft.TreasureCards.{Copper, Silver, Gold}
import Cards._

object CardDeserializationSpec extends Specification {

  "treasure cards" should {
    "deserialize" in {
      fromWire("Copper") must_==Copper()
      fromWire("Silver") must_==Silver()
      fromWire("Gold") must_==Gold()
    }
  }

  "action cards" should {
    "deserialize" in {
      fromWire("Witch") must_==Witch()
      fromWire("Remodel") must_==Remodel()
    }
  }

  "deserialized cards of the same time" should {
    "be the same instance" in {
      fromWire("Copper") mustEq(fromWire("Copper"))
    }
  }

}