package org.grumpysoft

object SingleGame {
  def apply(players: List[GenericPlayer[Card]], supply: Supply) : Seq[(SelfDescribing, Int)] = {
    val initialGame = Game.standardGame(players, supply)
    val endGame = turns(initialGame).dropWhile(!_.isOver).head
    Scorer.leaderboard(endGame.table)
  }

  private def turns(startGame: Game) : Stream[Game] = {
    Stream.iterate(startGame)(game => game.takeTurn)
  }
}