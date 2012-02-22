package org.grumpysoft

import scala.collection.mutable.Stack

import org.specs2.mutable.Specification

object UniquererSpec extends Specification {
  "uniquerer" should {
    "settify a sequence" in {
      uniquery.toSet(List(1,1,1,3,4,5,4,6)) must_==List(1,3,4,5,6)
    }
  }
}








