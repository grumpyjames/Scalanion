package org.grumpysoft

import actioncards.{Remodel, Witch}
import org.specs.Specification
import scalaj.collection.Imports._
import org.specs.mock.Mockito
import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import collection.Seq
import java.lang.String
import org.grumpysoft.TreasureCards._
import collection.immutable.List
import org.grumpysoft.Scalanion.{ChooseForOtherPlayer => ProtoCfop, _}

case class RemotePlayer(private val player: GenericPlayer[Int], private val input: InputStream, private val output: OutputStream) {
  def passEvent(message: Scalanion.ServerToClient): Unit = {
    val event = message.getEvent
    val seq: Seq[String] = event.getCardList.asScala
    player.playerEvent(liftPlayer(event.getPlayer), liftVerb(event.getVerb), liftCards(seq))
  }

  def passHand(message: Scalanion.ServerToClient) : Unit = {
    val hand = message.getHand
    player.newHand(liftCards(hand.getCardList.asScala))
  }

  def passChooseFrom(message: Scalanion.ServerToClient) : Unit = {
    val chooseFrom = message.getChooseFrom
    val responses: scala.Seq[Int] = player.chooseFrom(liftCards(chooseFrom.getCardList.asScala), liftVerb(chooseFrom.getVerb), chooseFrom.getMinimumChoices, chooseFrom.getMaximumChoices)
    Choices.newBuilder.addAllChoice(responses.asJava).build.writeDelimitedTo(output)
  }

  def sendResponse(response: Boolean): Any = {
    Answer.newBuilder.setAnswer(response).build.writeDelimitedTo(output)
  }

  def passQuery(message: Scalanion.ServerToClient) : Unit = {
    val query = message.getQuery
    if (query.hasQuestion) {
      sendResponse(player.query(BasicQuestion(query.getQuestion)))
    } else {
      val cfop = query.getChooseForOtherPlayer
      sendResponse(player.query(ChooseForOtherPlayer(liftCards(cfop.getCardList.asScala), StringDescription(cfop.getPlayer), liftVerb(cfop.getVerb))))
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
    return StringDescription(playerName)
  }

  private def liftVerb(presentTense: String) : Verb = {
    Verbs.fromWire(presentTense)
  }

  private def liftCards(cards: Seq[String]) : List[Card] = {
    cards.map(Cards.fromWire(_)).toList
  }
}

