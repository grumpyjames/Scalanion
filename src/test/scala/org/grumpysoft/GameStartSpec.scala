package org.grumpysoft

object GameStartSpec extends org.specs2.mutable.Specification with org.specs2.mock.Mockito {
  
  trait TestState extends org.specs2.specification.Scope {
    val supply = mock[Supply]
    
    supply.gameOver returns false

    def makeGame(players: List[GenericPlayer[Card]]) : Game = {
      Game(players, supply, BoringBuyPhase(), BoringActionPhase())
    }

    def checkStandardHand(player: SinkPlayer) = {
      player.hands.flatten.groupBy(_.describe()).map(a => (a._1, a._2.size)).toList.sortBy(_._2) must_==List(("Estate", 3),("Copper",7))
    }
  }

  case class BoringBuyPhase() extends BuyPhaseFn {
    def doBuyPhase(buys: Int, treasure: Int, player: GenericPlayer[Card], supply: Supply) = {
      (supply, Nil)
    }
  }

  case class BoringActionPhase() extends ActionPhaseFn {
    def doActionPhase(stacks: Stacks, player: GenericPlayer[Card], supply: Supply, table: Table) : ActionResult = {
      ActionResult.noTreasureOrBuysOrActions(stacks, supply, table)
    }
  }

  "a game, when started" should {
    "with a single player, after a turn, updated the player with 7 coppers and 3 estates" in new TestState {
      val player = new SinkPlayer
      val game = makeGame(List(player))
      game.takeTurn
      checkStandardHand(player)
    }
    "with four players, after a turn each, have given seven coppers and estates to each" in new TestState {
      val players = List.fill(4)(new SinkPlayer())
      var game = makeGame(players)
      0.until(4).foreach(a => game = game.takeTurn)
      players.map(checkStandardHand(_)).reduceLeft(_ and _)
    }
  }

}