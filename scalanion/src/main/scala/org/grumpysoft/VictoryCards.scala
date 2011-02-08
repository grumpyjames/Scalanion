package org.grumpysoft

object VictoryCards {
  object Estate {
    def apply() : Estate = { new Estate }
  }

  class Estate extends VictoryCard(2,1) {
    def describe() : String = { "Estate" }
  }

  object Province {
    def apply() : Province = { new Province }
  }

  class Province extends VictoryCard(8,6) {
    def describe() : String = { "Province" }
  }
}
