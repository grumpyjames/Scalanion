package org.grumpysoft

import org.specs.Specification

import TreasureCards._

object CardSpec extends Specification {
  "any card" should {
    "multiply itself correctly" in {
      Copper().times(10) must_==List(Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Copper(), Copper())
    }
  }
}