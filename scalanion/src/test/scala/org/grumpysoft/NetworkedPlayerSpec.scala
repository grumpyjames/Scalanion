package org.grumpysoft

import org.specs.Specification
import scalaj.collection.Imports._
import java.io.{OutputStream, ByteArrayInputStream, InputStream, ByteArrayOutputStream}
import org.grumpysoft.TreasureCards.{Copper, Silver}
import org.specs.mock.Mockito
import org.grumpysoft.Scalanion.{Hand, Query => ProtobufQuery, ChooseFrom, ChooseForOtherPlayer => ProtobufChooseForOtherPlayer, Answer, Choices, Event}

object NetworkedPlayerSpec extends Specification with Mockito {

  val mockOtherPlayer = mock[GenericPlayer[Any]]

  def buildInput(mutator: (OutputStream => Unit)) : InputStream = {
    val scratchSpace = new ByteArrayOutputStream(128)
    mutator(scratchSpace)
    toInput(scratchSpace)
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

  def toInput(baos: ByteArrayOutputStream) : InputStream = {
    new ByteArrayInputStream(baos.toByteArray)
  }

  def noInput() : InputStream = {
    buildInput(ss => {})
  }

  "network player" should {
    val output = new ByteArrayOutputStream(2048)

    "transmit choices and retrieve responses" in {
      val networkPlayer = NetworkPlayer(buildResponsesInput, output)

      networkPlayer.chooseFrom(List(Copper(), Silver(), Silver(), Copper()), Discard, 0, 2) must_==List(0, 3)
      networkPlayer.query(BasicQuestion("are you geoff?")) must_==true

      val networkPlayerOutput = toInput(output)

      val transmitted = ChooseFrom.parseDelimitedFrom(networkPlayerOutput)
      transmitted.getCardList.asScala must_==List("Copper", "Silver", "Silver", "Copper")
      transmitted.getMinimumChoices must_==0
      transmitted.getMaximumChoices must_==2
      transmitted.getVerb must_=="discard"
      val transmittedQuery = ProtobufQuery.parseDelimitedFrom(networkPlayerOutput)
      transmittedQuery.getQuestion must_=="are you geoff?"
    }

    "transmit queries corresponding to choices for other players" in {
      val networkPlayer = NetworkPlayer(falseResponseInput, output)
      mockOtherPlayer.describe returns "another player"
      networkPlayer.query(ChooseForOtherPlayer(List(Copper(), Silver()), mockOtherPlayer, Gain)) must_==false
      val transmitted = ProtobufChooseForOtherPlayer.parseDelimitedFrom(toInput(output))
      transmitted.getPlayer must_=="another player"
      transmitted.getVerb must_=="gain"
      transmitted.getCardList.asScala must_==List("Copper", "Silver")
    }

    "transmit hand contents" in {
      val networkPlayer = NetworkPlayer(noInput, output)
      networkPlayer.newHand(List(Copper(), Silver(), Copper()))
      val transmitted = Hand.parseDelimitedFrom(toInput(output))
      transmitted.getCardList.asScala must_==List("Copper", "Silver", "Copper")
    }

    "transmit player events" in {
      val networkPlayer = NetworkPlayer(noInput, output)
      mockOtherPlayer.describe returns "another player"
      networkPlayer.playerEvent(mockOtherPlayer, Discard, List(Copper(), Silver()))
      val transmitted = Event.parseDelimitedFrom(toInput(output))
      transmitted.getVerb() must_==Discard.present
      transmitted.getPlayer() must_=="another player"
      transmitted.getCardList.asScala must_==List("Copper", "Silver")
    }
  }
}

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