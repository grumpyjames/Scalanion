package org.grumpysoft

import org.specs.Specification
import scalaj.collection.Imports._
import org.grumpysoft.Scalanion.{ChooseFrom, Answer, Choices}
import java.io.{OutputStream, ByteArrayInputStream, InputStream, ByteArrayOutputStream}
import org.grumpysoft.TreasureCards.{Copper, Silver}

object NetworkedPlayerSpec extends Specification {
  val output = new ByteArrayOutputStream(2048)
  val input = buildResponsesInput()

  def buildResponsesInput() : InputStream = {
    val scratchSpace = new ByteArrayOutputStream(128)
    Choices.newBuilder.addChoice(0).addChoice(3).build.writeDelimitedTo(scratchSpace)
    Answer.newBuilder.setAnswer(true).build.writeTo(scratchSpace)
    toInput(scratchSpace)
  }

  def toInput(baos: ByteArrayOutputStream) : InputStream = {
    new ByteArrayInputStream(baos.toByteArray)
  }

  val networkPlayer = NetworkPlayer(input, output)

  "network player" should {
    "transmit choices correctly, and retrieve the response" in {
      networkPlayer.chooseFrom(List(Copper(), Silver(), Silver(), Copper()), Discard, 0, 2) must_==List(0, 3)
      val transmitted = ChooseFrom.parseFrom(toInput(output))
      transmitted.getCardList.asScala must_==List("Copper", "Silver", "Silver", "Copper")
      transmitted.getMinimumChoices must_==0
      transmitted.getMaximumChoices must_==2
      transmitted.getVerb must_=="discard"
    }
  }
}

case class NetworkPlayer(private val input: InputStream, private val output: OutputStream) extends GenericPlayer[Int] {
  def describe() = "Network Player"

  def playerEvent(player: GenericPlayer[Any], action: Verb, cards: Seq[Card]) = {

  }

  def newHand(hand: Seq[Card]) = {

  }

  def query(question: Query) = {
    false
  }

  def chooseFrom(cards: Seq[Card], purpose: Verb, minChoices: Int, maxChoices: Int) : Seq[Int] = {
    val builder = ChooseFrom.newBuilder.addAllCard(cards.map(_.describe).asJava).setVerb(purpose.present).setMinimumChoices(minChoices).setMaximumChoices(maxChoices)
    builder.build.writeTo(output)
    Choices.parseDelimitedFrom(input).getChoiceList.asScala
  }
}