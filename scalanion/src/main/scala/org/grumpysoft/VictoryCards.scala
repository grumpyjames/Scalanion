package org.grumpysoft

object VictoryCards {
  object Estate {
    def apply() : Estate = { new Estate }
  }

  class Estate extends VictoryCard(2,1) {
    def describe() : String = { "Estate" }
    protected def copyThyself() = Estate()
  }

  object Duchy {
    def apply() : Duchy = { new Duchy }
  }

  class Duchy extends VictoryCard(5,3) {
    def describe() : String = { "Duchy" }
    protected def copyThyself() = Duchy()
  }

  object Province {
    def apply() : Province = { new Province }
  }

  class Province extends VictoryCard(8,6) {
    def describe() : String = { "Province" }
    protected def copyThyself() = Province()
  }
}
