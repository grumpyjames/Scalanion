package org.grumpysoft

object Server {
  def main(args: Array[String]) = {
    val completeLobby = Lobby(8080).waitFor(2)
    completeLobby.close
    val players = completeLobby.players.map(new RichPlayer(_))

    players.foreach(_.gameEvent(Start(System.currentTimeMillis)))

    val game = Game.standardGame(players, Supplies.forTwo)

    val endGame = turns(game).dropWhile(!_.isOver)
    players.foreach(_.gameEvent(End(endGame.head.leaderboard)))
  }

  private def turns(startGame: Game) : Stream[Game] = {
    Stream.iterate(startGame)(game => game.takeTurn)
  }
}