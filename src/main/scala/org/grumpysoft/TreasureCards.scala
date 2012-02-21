package org.grumpysoft
    
object TreasureCards {

  object Copper {
    def apply() : Copper = { new Copper }
  }

  object Silver {
    def apply() : Silver = { new Silver }
  }

  object Gold {
    def apply() : Gold = { new Gold }
  }
  
  class Copper extends TreasureCard(0,1) {
    def describe() : String = { "Copper" }
    protected def copyThyself() = Copper()
  }

  class Silver extends TreasureCard(3,2) {
    def describe() : String = { "Silver" }
    protected def copyThyself() = Silver()
  }

  class Gold extends TreasureCard(6,3) {
    def describe() : String = { "Gold" }
    protected def copyThyself() = Gold()
  }

}
