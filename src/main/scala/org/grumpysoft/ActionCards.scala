package org.grumpysoft

object ActionCards {

  object Remodel {
    def apply() : Remodel = {
      new Remodel
    }
  }

  class Remodel extends ActionCard(4) {
    def play() : Unit = {}
    def describe() : String = { "Remodel" }
  }

  object Witch {
    def apply() = { new Witch }
  }

  class Witch extends ActionCard(5) {
    def play() : Unit = {}
    def describe() : String = { "Witch" }
  }

  object Militia {
    def apply() = { new Militia }
  }

  class Militia extends ActionCard(4) {
    def play() : Unit = {}
    def describe() : String = { "Militia" }
  }

}
