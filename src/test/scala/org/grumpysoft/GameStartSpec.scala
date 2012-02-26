package org.grumpysoft

object GameStartSpec extends org.specs2.mutable.Specification with org.specs2.mock.Mockito {
  
  trait TestState extends GameSpecState {
    val supply = mock[Supply]
    
    supply.gameOver returns false

    def makeGame(players: List[GenericPlayer[Card]]) : Game = {
      Game(players, supply, BoringBuyPhase(), BoringActionPhase())
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