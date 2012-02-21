package org.grumpysoft

import org.specs2.mutable.Specification

object OptionFormatterSpec extends Specification {
  val unformatted = List(StringDescription("forum"),StringDescription("quorum"),StringDescription("no"))
  "an option formatter when asked to format some options" should {
    "number and format them nicely" in {
      val formatted = new OptionFormatter().format(unformatted).map(_.describe)
      formatted must_==List("1. forum", "2. quorum", "3. no")
    }
  }
}
