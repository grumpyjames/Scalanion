package org.grumpysoft

import java.io.{InputStream, OutputStream}
import collection.Seq
import java.lang.String
import collection.immutable.List
import scala.collection.JavaConversions._
import org.grumpysoft.pb.Scalanion._

object RemotePlayer {

  def from(player: GenericPlayer[Int], input: InputStream, output: OutputStream) : RemotePlayer = {
    Introduction.newBuilder.setPlayerName(player.describe()).build.writeDelimitedTo(output)
    RemotePlayer(player, input, output)
  }

}

case class RemotePlayer(private val player: GenericPlayer[Int], private val input: InputStream, private val output: OutputStream) {

  private def passEvent(message: ServerToClient) {
    val event = message.getEvent
    val seq: Seq[String] = event.getCardList
    player.playerEvent(liftPlayer(event.getPlayer), liftVerb(event.getVerb), liftCards(seq))
  }

  private def passHand(message: ServerToClient) {
    val hand = message.getHand
    player.newHand(liftCards(hand.getCardList))
  }

  private def passChooseFrom(message: ServerToClient) {
    val chooseFrom = message.getChooseFrom
    val responses: Seq[Int] = player.chooseFrom(liftCards(chooseFrom.getCardList),
      liftVerb(chooseFrom.getVerb), chooseFrom.getMinimumChoices, chooseFrom.getMaximumChoices)
    Choices.newBuilder.addAllChoice(responses.map(a => Int.box(a))).build.writeDelimitedTo(output)
  }

  private def sendResponse(response: Boolean): Any = {
    Answer.newBuilder.setAnswer(response).build.writeDelimitedTo(output)
  }

  private def passQuery(message: ServerToClient) {
    val query = message.getQuery
    if (query.hasQuestion) {
      sendResponse(player.query(BasicQuestion(query.getQuestion)))
    } else {
      val cfop = query.getChooseForOtherPlayer
      sendResponse(player.query(ChooseForOtherPlayer(liftCards(cfop.getCardList), liftPlayer(cfop.getPlayer), liftVerb(cfop.getVerb))))
    }
  }

  private def passGameEvent(message: ServerToClient) {
    val gameEvent = message.getGameEvent
    if (gameEvent.hasStart) {
      player.gameEvent(Start(gameEvent.getStart.getStartTime))
    } else {
      val playersWithScores = gameEvent.getGameOver.getPlayerWithScoreList
      player.gameEvent(End(playersWithScores.map(a => (StringDescription(a.getPlayerName), a.getScore))))
    }
  }

  def readAndForward() {
    val message = ServerToClient.parseDelimitedFrom(input)
    if (message.hasEvent) passEvent(message)
    if (message.hasHand) passHand(message)
    if (message.hasChooseFrom) passChooseFrom(message)
    if (message.hasQuery) passQuery(message)
    if (message.hasGameEvent) passGameEvent(message)
  }

  private def liftPlayer(playerName: String) : SelfDescribing = {
    StringDescription(playerName)
  }

  private def liftVerb(presentTense: String) : Verb = {
    Verbs.fromWire(presentTense)
  }

  private def liftCards(cards: Seq[String]) : List[Card] = {
    cards.map(Cards.fromWire(_)).toList
  }
}

