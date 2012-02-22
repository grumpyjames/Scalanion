package org.grumpysoft

import org.specs2.mutable.Specification
import scala.collection.JavaConversions._
import java.io.{OutputStream, ByteArrayInputStream, InputStream, ByteArrayOutputStream}
import org.grumpysoft.TreasureCards.{Copper, Silver}
import org.specs2.mock.Mockito
import org.grumpysoft.pb.Scalanion.{ServerToClient, Answer, Choices}

object NetworkPlayerSpec extends Specification with Mockito {

  trait TestState extends org.specs2.specification.Scope {
    val mockOtherPlayer = mock[GenericPlayer[Any]]

    def buildInput(mutator: (OutputStream => Unit)) : InputStream = {
      val scratchSpace = new ByteArrayOutputStream(128)
      mutator(scratchSpace)
      toInput(scratchSpace)
    }

    def toInput(baos: ByteArrayOutputStream) : InputStream = {
      new ByteArrayInputStream(baos.toByteArray)
    }

    def buildResponsesInput() : InputStream = {
      buildInput({scratchSpace =>
        Choices.newBuilder.addChoice(0).addChoice(3).build.writeDelimitedTo(scratchSpace)
        Answer.newBuilder.setAnswer(true).build.writeDelimitedTo(scratchSpace)
      })
    }

    def falseResponseInput() : InputStream = {
      buildInput(scratchSpace => Answer.newBuilder.setAnswer(false).build.writeDelimitedTo(scratchSpace))
    }

    def noInput() : InputStream = {
      buildInput(ss => {})
    }

    def makeNetworkPlayer(in: InputStream, out: OutputStream) = {
      NetworkPlayer("test player", in, out)
    }

    val output = new ByteArrayOutputStream(2048)
  }

  "network player" should {
    "transmit two choices and retrieve two responses" in new TestState {
      val networkPlayer = makeNetworkPlayer(buildResponsesInput(), output)

      networkPlayer.chooseFrom(List(Copper(), Silver(), Silver(), Copper()), Discard, 0, 2) must_==List(0, 3)
      networkPlayer.query(BasicQuestion("are you geoff?")) must_==true

      val networkPlayerOutput = toInput(output)

      val transmitted = ServerToClient.parseDelimitedFrom(networkPlayerOutput).getChooseFrom
      val transmittedQuery = ServerToClient.parseDelimitedFrom(networkPlayerOutput).getQuery
      (transmitted.getCardList.map(a => a) must_==List("Copper", "Silver", "Silver", "Copper")) and
        (transmitted.getMinimumChoices must_==0) and
        (transmitted.getMaximumChoices must_==2) and
        (transmitted.getVerb must_=="discard") and
        (transmittedQuery.getQuestion must_=="are you geoff?")
    }

    "transmit queries corresponding to choices for other players" in new TestState {
      val networkPlayer = makeNetworkPlayer(falseResponseInput(), output)
      mockOtherPlayer.describe returns "another player"
      networkPlayer.query(ChooseForOtherPlayer(List(Copper(), Silver()), mockOtherPlayer, Gain)) must_==false
      val transmitted = ServerToClient.parseDelimitedFrom(toInput(output)).getQuery.getChooseForOtherPlayer
      (transmitted.getPlayer must_=="another player") and (transmitted.getVerb must_=="gain") and (transmitted.getCardList.map(a => a) must_==List("Copper", "Silver"))
    }

    "transmit hand contents" in new TestState {
      val networkPlayer = makeNetworkPlayer(noInput(), output)
      networkPlayer.newHand(List(Copper(), Silver(), Copper()))
      val transmitted = ServerToClient.parseDelimitedFrom(toInput(output)).getHand
      transmitted.getCardList.map(a => a) must_==List("Copper", "Silver", "Copper")
    }

    
    "transmit player events" in new TestState {
      val networkPlayer = makeNetworkPlayer(noInput(), output)
      mockOtherPlayer.describe returns "another player"
      networkPlayer.playerEvent(mockOtherPlayer, Discard, List(Copper(), Silver()))
      val transmitted = ServerToClient.parseDelimitedFrom(toInput(output)).getEvent
      (transmitted.getVerb must_==Discard.present) and (transmitted.getPlayer must_=="another player") and
        (transmitted.getCardList.map(a => a) must_==List("Copper", "Silver"))
    }

    "quietly swallow events concerning no cards" in new TestState {
      val networkPlayer = makeNetworkPlayer(noInput(), output)
      mockOtherPlayer.describe returns "moo"
      networkPlayer.playerEvent(mockOtherPlayer, Discard, Nil)
      output.toByteArray.length must_==0
    }

    "transmit a start event" in new TestState {
      val networkPlayer = makeNetworkPlayer(noInput(), output)
      val currentTime = System.currentTimeMillis()
      networkPlayer.gameEvent(Start(currentTime))
      val transmitted = ServerToClient.parseDelimitedFrom(toInput(output)).getGameEvent.getStart
      transmitted.getStartTime must_==currentTime
    }

    "transmit a game over event" in new TestState {
      val networkPlayer = makeNetworkPlayer(noInput(), output)
      val leaderBoard = List((StringDescription("foo"), 12), (StringDescription("bar"), 15))
      networkPlayer.gameEvent(End(leaderBoard))
      val transmitted = ServerToClient.parseDelimitedFrom(toInput(output)).getGameEvent.getGameOver
      val playerList = transmitted.getPlayerWithScoreList
      (playerList.map(_.getPlayerName) must_==leaderBoard.map(_._1.describe())) and (playerList.map(_.getScore) must_==leaderBoard.map(_._2))
    }

    // FIXME: need some tests that show behaviour with bad streams, protobuf occasionally spits out very poor error messages.
  }
}

