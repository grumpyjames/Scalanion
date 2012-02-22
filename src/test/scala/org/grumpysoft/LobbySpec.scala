package org.grumpysoft

import org.specs2.mutable.Specification
import scala.collection.JavaConversions._
import java.util.concurrent.CopyOnWriteArrayList
import java.net.Socket
import org.grumpysoft.pb.Scalanion.Introduction

object LobbySpec extends Specification {

  trait TestState extends org.specs2.specification.Scope {
    val playerSockets = new CopyOnWriteArrayList[Socket]

    val lobbyPort = 9090

    def connectTwoPlayers() {
      runInOtherThread(new Runnable() {
        def run = {
          playerSockets.add(new Socket("localhost", lobbyPort))
          playerSockets.add(new Socket("localhost", lobbyPort))
        }
      })
    }

    def runInOtherThread(r : Runnable) {
      val otherThread = new Thread(r)
      otherThread.start
      otherThread.join
    }

    def sendPlayerIntroductions(names: List[String]) {
      runInOtherThread (new Runnable() {
        def run() {
          playerSockets.zip(names).map(a => Introduction.newBuilder().setPlayerName(a._2).build.writeDelimitedTo(a._1.getOutputStream))
        }
      })
    }

    val playerNames = List("geoff", "peter")
  }

  sequential

  "the lobby" should {

    "treat incoming connections as players" in new TestState {
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
    "work out the connected players' names from their introductions" in new TestState {
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