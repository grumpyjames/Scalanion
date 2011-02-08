package org.grumpysoft

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec

class OptionFormatterTest extends WordSpec with ShouldMatchers {
  val unformatted = List(StringDescription("forum"),StringDescription("quorum"),StringDescription("no"))
  "an option formatter" when {
    "asked to format some options" should {
      "number and format them nicely" in {
	val formatted = new OptionFormatter().format(unformatted).map(_.describe)
	formatted should equal (List("1. forum", "2. quorum", "3. no"))	    
      }
    }
  }
}
