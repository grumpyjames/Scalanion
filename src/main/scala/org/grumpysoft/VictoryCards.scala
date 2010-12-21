package org.grumpysoft

object VictoryCards {
  case class Estate() extends VictoryCard {
    def cost() : Int = { 2 }
    def victoryPoints(): Int = { 1 }
    def describe() : String = { "Estate" }
  }
}
