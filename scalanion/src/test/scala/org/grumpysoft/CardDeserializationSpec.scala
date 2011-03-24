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
      fromWire("Witch") must_==Witch().toActionCard
      fromWire("Remodel") must_==Remodel().toActionCard
    }
  }

  "deserialized cards of the same time" should {
    "not be the same instance" in {
      fromWire("Copper") mustNotEq(fromWire("Copper"))
    }
  }

  "unknown cards" should {
    "fail gracefully" in {
      try {
        fromWire("Oops")
      } catch {
        case re: RuntimeException => {
          re.getMessage must_=="Card not found: Oops"
        }
        case e => { throw e }
      }
    }
  }

}