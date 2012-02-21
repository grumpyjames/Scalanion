package org.grumpysoft

import java.io.{OutputStream, InputStream}
import org.grumpysoft.pb.Scalanion.{ServerToClient, Hand, Query => ProtobufQuery, ChooseFrom, ChooseForOtherPlayer => ProtobufChooseForOtherPlayer, GameEvent => PBGameEvent, Answer, Choices, Event, GameStart => PBStart, GameOver, PlayerWithScore}
import scala.collection.JavaConversions._

case class NetworkPlayer(private val name: String, private val input: InputStream, private val output: OutputStream) extends GenericPlayer[Int] {
  def describe() = name

  def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) {
    if (!cards.isEmpty)
      send(Event.newBuilder.setPlayer(player.describe()).setVerb(action.present).addAllCard(cards.map(_.describe())).build)
  }

  def newHand(hand: Seq[Card]) {
    send(Hand.newBuilder.addAllCard(hand.map(_.describe())).build)
  }

  def query(question: Query) : Boolean = {
    sendQuery(question)
    Answer.parseDelimitedFrom(input).getAnswer
  }

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
    send(ChooseFrom.newBuilder.addAllCard(cards.map(_.describe())).setVerb(purpose.present).setMinimumChoices(minChoices).setMaximumChoices(maxChoices).build)
    Choices.parseDelimitedFrom(input).getChoiceList.map(_.intValue())
  }

  private def sendQuery(question: Query) {
    question match {
      case BasicQuestion(str) => sendWrappedQuery(str)
      case ChooseForOtherPlayer(cards, player, verb) => sendWrappedQuery(cards, player, verb)
    }
  }

  private def sendWrappedQuery(rawQuery: String) {
    send(ProtobufQuery.newBuilder.setQuestion(rawQuery).build)
  }

  private def makeCfop(cards: scala.Seq[Card], verb: Verb, player: SelfDescribing): ProtobufChooseForOtherPlayer = {
    ProtobufChooseForOtherPlayer.newBuilder.addAllCard(cards.map(_.describe())).setVerb(verb.present).setPlayer(player.describe()).build
  }

  private def sendWrappedQuery(cards: Seq[Card], player: SelfDescribing, verb: Verb) {
    send(ProtobufQuery.newBuilder
      .setChooseForOtherPlayer(makeCfop(cards, verb, player))
      .build)
  }

  private def send(query: ProtobufQuery) {
    ServerToClient.newBuilder.setQuery(query).build.writeDelimitedTo(output)
  }

  private def send(hand: Hand) {
    ServerToClient.newBuilder.setHand(hand).build.writeDelimitedTo(output)
  }

  private def send(event: Event) {
    ServerToClient.newBuilder.setEvent(event).build.writeDelimitedTo(output)
  }

  private def send(chooseFrom: ChooseFrom) {
    ServerToClient.newBuilder.setChooseFrom(chooseFrom).build.writeDelimitedTo(output)
  }

  private def send(gameEvent: PBGameEvent) {
    ServerToClient.newBuilder.setGameEvent(gameEvent).build.writeDelimitedTo(output)
  }

  private def buildGameOver(leaderboard: Seq[(SelfDescribing, Int)]) : GameOver = {
    val playerWithScores = leaderboard.map(pws =>
      PlayerWithScore.newBuilder.setPlayerName(pws._1.describe()).setScore(pws._2).build
					 )
    GameOver.newBuilder.addAllPlayerWithScore(playerWithScores).build      
  }

  def gameEvent(event: GameEvent) {
    event match {
      case Start(startTime) => send(PBGameEvent.newBuilder.setStart(PBStart.newBuilder.setStartTime(startTime)).build)
      case End(leaderBoard) => send(PBGameEvent.newBuilder.setGameOver(buildGameOver(leaderBoard)).build)
    }
  }
}

