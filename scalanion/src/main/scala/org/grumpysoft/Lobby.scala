package org.grumpysoft

import java.net.{ServerSocket, Socket}
import org.grumpysoft.Scalanion.Introduction

object Lobby {
  def apply(port: Int) : Lobby = {
    Lobby(new ServerSocket(port), List[GenericPlayer[Int]]())
  }
}

// essentially, this is a NetworkPlayerFactory
case class Lobby(serverSocket: ServerSocket, players: List[GenericPlayer[Int]]) {
  def waitFor(connectionCount: Int) : Lobby = connectionCount match {
    case 0 => Lobby(serverSocket, players.reverse)
    case a => Lobby(serverSocket, makePlayer(serverSocket.accept) :: players).waitFor(connectionCount - 1)
  }
  def close() = { serverSocket.close }

  private def makePlayer(socket: Socket) : GenericPlayer[Int] = {
    val in = socket.getInputStream
    val name = Introduction.parseDelimitedFrom(in).getPlayerName
    NetworkPlayer(name, in, socket.getOutputStream)
  }
}



