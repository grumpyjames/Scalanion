package org.grumpysoft

import org.specs.Specification
import scalaj.collection.Imports._
import java.util.concurrent.CopyOnWriteArrayList
import java.net.Socket
import org.grumpysoft.Scalanion.Introduction

object LobbySpec extends Specification {

  val playerSockets = new CopyOnWriteArrayList[Socket]

  val lobbyPort = 9090

  def connectTwoPlayers: Unit = {
    runInOtherThread(new Runnable() {
      def run = {
        playerSockets.add(new Socket("localhost", lobbyPort))
        playerSockets.add(new Socket("localhost", lobbyPort))
      }
    })
  }

  def runInOtherThread(r : Runnable) : Unit = {
    val otherThread = new Thread(r)
    otherThread.start
    otherThread.join
  }

  def sendPlayerIntroductions(names: List[String]) : Unit = {
    runInOtherThread (new Runnable() {
      def run = {
        playerSockets.asScala.zip(names).map(a => Introduction.newBuilder().setPlayerName(a._2).build.writeDelimitedTo(a._1.getOutputStream))
      }
    })
  }

  "the lobby" should {
    val playerNames = List("geoff", "peter")
    "treat incoming connections as players" in {
      val lobby = Lobby(lobbyPort)
      try {
        connectTwoPlayers
        sendPlayerIntroductions(playerNames)
        val finishedLobby = lobby.waitFor(2)
        finishedLobby.players.size must_==2
      } finally {
        lobby.close
      }
    }
    "work out the connected players' names from their introductions" in {
      val lobby = Lobby(lobbyPort)
      try {
        connectTwoPlayers
        sendPlayerIntroductions(playerNames)
        val finishedLobby = lobby.waitFor(2)
        finishedLobby.players.map(_.describe) must_==playerNames
      } finally {
        lobby.close
      }
    }
  }
}