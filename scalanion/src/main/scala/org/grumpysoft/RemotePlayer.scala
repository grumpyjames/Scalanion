package org.grumpysoft

import scalaj.collection.Imports._
import java.io.{InputStream, OutputStream}
import collection.Seq
import java.lang.String
import collection.immutable.List
import org.grumpysoft.Scalanion._

case class RemotePlayer(private val player: GenericPlayer[Int], private val input: InputStream, private val output: OutputStream) {

  private def passEvent(message: Scalanion.ServerToClient): Unit = {
    val event = message.getEvent
    val seq: Seq[String] = event.getCardList.asScala
    player.playerEvent(liftPlayer(event.getPlayer), liftVerb(event.getVerb), liftCards(seq))
  }

  private def passHand(message: Scalanion.ServerToClient) : Unit = {
    val hand = message.getHand
    player.newHand(liftCards(hand.getCardList.asScala))
  }

  private def passChooseFrom(message: Scalanion.ServerToClient) : Unit = {
    val chooseFrom = message.getChooseFrom
    val responses: scala.Seq[Int] = player.chooseFrom(liftCards(chooseFrom.getCardList.asScala), liftVerb(chooseFrom.getVerb), chooseFrom.getMinimumChoices, chooseFrom.getMaximumChoices)
    Choices.newBuilder.addAllChoice(responses.asJava).build.writeDelimitedTo(output)
  }

  private def sendResponse(response: Boolean): Any = {
    Answer.newBuilder.setAnswer(response).build.writeDelimitedTo(output)
  }

  private def passQuery(message: Scalanion.ServerToClient) : Unit = {
    val query = message.getQuery
    if (query.hasQuestion) {
      sendResponse(player.query(BasicQuestion(query.getQuestion)))
    } else {
      val cfop = query.getChooseForOtherPlayer
      sendResponse(player.query(ChooseForOtherPlayer(liftCards(cfop.getCardList.asScala), liftPlayer(cfop.getPlayer), liftVerb(cfop.getVerb))))
    }
  }

  def readAndForward : Unit = {
    val message = ServerToClient.parseDelimitedFrom(input)
    if (message.hasEvent) return passEvent(message)
    if (message.hasHand) return passHand(message)
    if (message.hasChooseFrom) return passChooseFrom(message)
    if (message.hasQuery) return passQuery(message)
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

