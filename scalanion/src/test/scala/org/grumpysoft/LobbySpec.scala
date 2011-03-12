package org.grumpysoft

import org.specs.Specification
import java.util.concurrent.CopyOnWriteArrayList
import java.net.{ServerSocket, Socket}

object LobbySpec extends Specification {

  val playerSockets = new CopyOnWriteArrayList[Socket]

  "the lobby" should {
    "treat incoming connections as players" in {
      val lobby = Lobby(9090)
      try {
        val connectingThread = new Thread(new Runnable() {
          def run = {
            playerSockets.add(new Socket("localhost", 9090))
            playerSockets.add(new Socket("localhost", 9090))
          }
        })
        connectingThread.start
        connectingThread.join
        val finishedLobby = lobby.waitFor(2)
        finishedLobby.players.size must_==2
      } finally {
        lobby.close
      }
    }
  }
}

object Lobby {
  def apply(port: Int) : Lobby = {
    Lobby(new ServerSocket(port), List[Socket]())
  }
}

case class Lobby(serverSocket: ServerSocket, players: List[Socket]) {
  def waitFor(connectionCount: Int) : Lobby = connectionCount match {
    case 0 => this
    case a => Lobby(serverSocket, serverSocket.accept :: players).waitFor(connectionCount - 1)
  }
  def close() = { serverSocket.close }
}