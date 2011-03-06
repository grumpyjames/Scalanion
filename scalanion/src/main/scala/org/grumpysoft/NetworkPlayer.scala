package org.grumpysoft

import scalaj.collection.Imports._
import java.io.{OutputStream, InputStream}
import org.grumpysoft.Scalanion.{Hand, Query => ProtobufQuery, ChooseFrom, ChooseForOtherPlayer => ProtobufChooseForOtherPlayer, Answer, Choices, Event}

case class NetworkPlayer(private val input: InputStream, private val output: OutputStream) extends GenericPlayer[Int] {
  def describe() = "Network Player"

  def playerEvent(player: GenericPlayer[Any], action: Verb, cards: Seq[Card]) = {
    Event.newBuilder.setPlayer(player.describe).setVerb(action.present).addAllCard(cards.map(_.describe).asJava).build.writeDelimitedTo(output)
  }

  def newHand(hand: Seq[Card]) = {
    Hand.newBuilder.addAllCard(hand.map(_.describe).asJava).build.writeDelimitedTo(output)
  }

  private def sendQuery(question: Query) : Unit = question match {
    case BasicQuestion(str) => ProtobufQuery.newBuilder.setQuestion(str).build.writeDelimitedTo(output)
    case ChooseForOtherPlayer(cards, player, verb) => ProtobufChooseForOtherPlayer.newBuilder.addAllCard(cards.map(_.describe).asJava).setVerb(verb.present).setPlayer(player.describe).build.writeDelimitedTo(output)
  }

  def query(question: Query) : Boolean = {
    sendQuery(question)
    Answer.parseDelimitedFrom(input).getAnswer
  }

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
    val builder = ChooseFrom.newBuilder.addAllCard(cards.map(_.describe).asJava).setVerb(purpose.present).setMinimumChoices(minChoices).setMaximumChoices(maxChoices)
    builder.build.writeDelimitedTo(output)
    Choices.parseDelimitedFrom(input).getChoiceList.asScala
  }
}

