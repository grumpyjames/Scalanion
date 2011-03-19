package org.grumpysoft

import scalaj.collection.Imports._
import java.io.{OutputStream, InputStream}
import org.grumpysoft.Scalanion.{ServerToClient, Hand, Query => ProtobufQuery, ChooseFrom, ChooseForOtherPlayer => ProtobufChooseForOtherPlayer, GameEvent => PBGameEvent, Answer, Choices, Event, Start => PBStart, GameOver, PlayerWithScore}

case class NetworkPlayer(private val name: String, private val input: InputStream, private val output: OutputStream) extends GenericPlayer[Int] {
  def describe() = name

  def playerEvent(player: SelfDescribing, action: Verb, cards: Seq[Card]) = {
    send(Event.newBuilder.setPlayer(player.describe).setVerb(action.present).addAllCard(cards.map(_.describe).asJava).build)
  }

  def newHand(hand: Seq[Card]) = {
    send(Hand.newBuilder.addAllCard(hand.map(_.describe).asJava).build)
  }

  def query(question: Query) : Boolean = {
    sendQuery(question)
    Answer.parseDelimitedFrom(input).getAnswer
  }

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
    send(ChooseFrom.newBuilder.addAllCard(cards.map(_.describe).asJava).setVerb(purpose.present).setMinimumChoices(minChoices).setMaximumChoices(maxChoices).build)
    Choices.parseDelimitedFrom(input).getChoiceList.asScala
  }

  private def sendQuery(question: Query) : Unit = question match {
    case BasicQuestion(str) => sendWrappedQuery(str)
    case ChooseForOtherPlayer(cards, player, verb) => sendWrappedQuery(cards, player, verb)
  }

  private def sendWrappedQuery(rawQuery: String) : Unit = {
    send(ProtobufQuery.newBuilder.setQuestion(rawQuery).build)
  }

  private def makeCfop(cards: scala.Seq[Card], verb: Verb, player: SelfDescribing): Scalanion.ChooseForOtherPlayer = {
    ProtobufChooseForOtherPlayer.newBuilder.addAllCard(cards.map(_.describe).asJava).setVerb(verb.present).setPlayer(player.describe).build
  }

  private def sendWrappedQuery(cards: Seq[Card], player: SelfDescribing, verb: Verb) : Unit = {
    send(ProtobufQuery.newBuilder
      .setChooseForOtherPlayer(makeCfop(cards, verb, player))
      .build)
  }

  private def send(query: ProtobufQuery) : Unit = {
    ServerToClient.newBuilder.setQuery(query).build.writeDelimitedTo(output)
  }

  private def send(hand: Hand) : Unit = {
    ServerToClient.newBuilder.setHand(hand).build.writeDelimitedTo(output)
  }

  private def send(event: Event) : Unit = {
    ServerToClient.newBuilder.setEvent(event).build.writeDelimitedTo(output)
  }

  private def send(chooseFrom: ChooseFrom) : Unit = {
    ServerToClient.newBuilder.setChooseFrom(chooseFrom).build.writeDelimitedTo(output)
  }

  private def send(gameEvent: PBGameEvent) : Unit = {
    ServerToClient.newBuilder.setGameEvent(gameEvent).build.writeDelimitedTo(output)
  }

  private def buildGameOver(leaderboard: Seq[(SelfDescribing, Int)]) : GameOver = {
    val playerWithScores = leaderboard.map(pws =>
      PlayerWithScore.newBuilder.setPlayerName(pws._1.describe).setScore(pws._2).build
					 ).asJava
    GameOver.newBuilder.addAllPlayerWithScore(playerWithScores).build      
  }

  def gameEvent(event: GameEvent) : Unit = event match {
    case Start(startTime) =>  send(PBGameEvent.newBuilder.setStart(PBStart.newBuilder.setStartTime(startTime)).build)
    case End(leaderBoard) => send(PBGameEvent.newBuilder.setGameOver(buildGameOver(leaderBoard)).build)
  }
}

