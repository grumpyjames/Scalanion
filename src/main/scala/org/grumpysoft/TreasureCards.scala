package org.grumpysoft
    
object TreasureCards {
  case class Copper() extends TreasureCard {
    def cost() : Int = { 0 }
    def value() : Int = { 1 }
    def describe() : String = { "Copper" }
  }
}
