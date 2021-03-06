package org.grumpysoft

import actioncards.{Remodel, Witch}
import org.specs2.mutable.Specification
import scala.collection.JavaConversions._
import org.specs2.mock.Mockito
import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import java.lang.String
import org.grumpysoft.TreasureCards._
import collection.immutable.List
import org.grumpysoft.pb.Scalanion.{ChooseForOtherPlayer => ProtoCfop, GameStart => PbStart, GameEvent => PbGameEvent, _}

object RemotePlayerSpec extends Specification with Mockito {

  trait TestState extends org.specs2.specification.Scope {
    val noOutput = new ByteArrayOutputStream(0)
    val realPlayer = mock[GenericPlayer[Int]]
    val choiceCards: List[ActionCard] = List(Witch(), Remodel(), Remodel()).map(_.toActionCard)

    val simpleQuery: String = "are you dave?"
    val justGold: List[Gold] = List(Gold())
    val playerDave: StringDescription = StringDescription("dave")

    def makeInput(streamWriter : (OutputStream => Unit)) = {
      val byteArrayOutput = new ByteArrayOutputStream(128)
      streamWriter(byteArrayOutput)
      inputFrom(byteArrayOutput)
    }

    def inputFrom(baos: ByteArrayOutputStream) : InputStream = {
      new ByteArrayInputStream(baos.toByteArray)
    }

    val thePlayer = "thePlayer"
    realPlayer.describe returns thePlayer

    def eventInput = {
      makeInput(ServerToClient.newBuilder.setEvent(Event.newBuilder.setPlayer(thePlayer).setVerb(Gain.present).addAllCard(List("Copper", "Silver"))).build.writeDelimitedTo(_))
    }

    def handInput = {
      makeInput(ServerToClient.newBuilder.setHand(Hand.newBuilder.addAllCard(List(Copper(), Witch().toActionCard, Gold(), Copper()).map(_.describe())).build).build.writeDelimitedTo(_))
    }

    def choiceInput = {
      makeInput(ServerToClient.newBuilder.setChooseFrom(ChooseFrom.newBuilder.setVerb(Play.present).setMinimumChoices(2).setMaximumChoices(2).addAllCard(choiceCards.map(_.describe())).build).build.writeDelimitedTo(_))
    }

    def queryInput = {
      makeInput({baos =>
        ServerToClient.newBuilder.setQuery(Query.newBuilder.setQuestion(simpleQuery)).build.writeDelimitedTo(baos)
        ServerToClient.newBuilder.setQuery(Query.newBuilder.setChooseForOtherPlayer(ProtoCfop.newBuilder.setPlayer(playerDave.describe())
          .setVerb(Trash.present).addAllCard(justGold.map(_.describe())))).build.writeDelimitedTo(baos)
      })
    }

    val theStartTime = 8613425L
    val theStart = PbGameEvent.newBuilder.setStart(PbStart.newBuilder.setStartTime(theStartTime).build).build

    val leaderboard = List( (StringDescription("foo"), 12), (StringDescription("bar"), 432))
    val playersWithScores = leaderboard.map(pws => PlayerWithScore.newBuilder.setPlayerName(pws._1.describe()).setScore(pws._2).build)
    val theGameOver = GameOver.newBuilder.addAllPlayerWithScore(playersWithScores).build
    val theLeaderboard = PbGameEvent.newBuilder.setGameOver(theGameOver)

    def gameEventInput = {
      makeInput({ baos =>
        ServerToClient.newBuilder.setGameEvent(theStart).build.writeDelimitedTo(baos)
        ServerToClient.newBuilder.setGameEvent(theLeaderboard).build.writeDelimitedTo(baos)
      })
    }

    val outputBuffer = new ByteArrayOutputStream(128)
  }

  "remote player" should {

    "introduce themselves" in new TestState {
      val remotePlayer = RemotePlayer.from(realPlayer, eventInput, outputBuffer)
      Introduction.parseDelimitedFrom(inputFrom(outputBuffer)).getPlayerName must_==thePlayer
    }

    "dispatch the received event to the wrapped player" in new TestState {
      val remotePlayer = RemotePlayer(realPlayer, eventInput, noOutput)
      remotePlayer.readAndForward()
      there was one(realPlayer).playerEvent(StringDescription(thePlayer), Gain, List(Copper(), Silver()))
    }

    "present the player with their new hand" in new TestState {
      val remotePlayer = RemotePlayer(realPlayer, handInput, noOutput)
      remotePlayer.readAndForward()
      there was one(realPlayer).newHand(List(Copper(), Witch(), Gold(), Copper()))
    }

    "present decisions to the player, and return the correct output" in new TestState {
      realPlayer.chooseFrom(choiceCards, Play, 2, 2) returns List(0, 2)
      val remotePlayer = RemotePlayer(realPlayer, choiceInput, outputBuffer)
      remotePlayer.readAndForward()
      there was one(realPlayer).chooseFrom(choiceCards, Play, 2, 2)
      Choices.parseDelimitedFrom(inputFrom(outputBuffer)).getChoiceList.map(a => a) must_==(List(0,2))
    }

    "retrieve answers to queries" in new TestState {
      realPlayer.query(BasicQuestion(simpleQuery)) returns false
      realPlayer.query(ChooseForOtherPlayer(justGold, playerDave, Trash)) returns true
      val remotePlayer = RemotePlayer(realPlayer, queryInput, outputBuffer)
      remotePlayer.readAndForward()
      remotePlayer.readAndForward()
      val responses = inputFrom(outputBuffer)
      (Answer.parseDelimitedFrom(responses).getAnswer must_==false) and (Answer.parseDelimitedFrom(responses).getAnswer must_==true)
    }

    "pass on game events" in new TestState {
      val remotePlayer = RemotePlayer(realPlayer, gameEventInput, noOutput)
      remotePlayer.readAndForward()
      remotePlayer.readAndForward()
      there was one(realPlayer).gameEvent(Start(theStartTime))
      there was one(realPlayer).gameEvent(End(leaderboard))
    }
  }

}

