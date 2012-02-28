package org.grumpysoft

object Server {
  def main(args: Array[String]) {
    val completeLobby = Lobby(8080).waitFor(1)
    completeLobby.close()
    val players = new RichPlayer(new TreasureOrProvincePlayer) :: completeLobby.players.map(new RichPlayer(_))

    players.foreach(_.gameEvent(Start(System.currentTimeMillis)))

    val game = Game.standardGame(players, Supplies.forTwo())

    val finishedGame = turns(game).dropWhile(!_.isOver).head
    players.foreach(_.gameEvent(End(Scorer.leaderboard(finishedGame.table))))
  }

  private def turns(startGame: Game) : Stream[Game] = {
    Stream.iterate(startGame)(game => game.takeTurn)
  }
}