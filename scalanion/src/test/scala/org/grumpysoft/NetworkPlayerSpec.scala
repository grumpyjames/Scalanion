package org.grumpysoft

import org.specs.Specification
import scalaj.collection.Imports._
import java.io.{OutputStream, ByteArrayInputStream, InputStream, ByteArrayOutputStream}
import org.grumpysoft.TreasureCards.{Copper, Silver}
import org.specs.mock.Mockito
import org.grumpysoft.Scalanion.{Hand, Query => ProtobufQuery, ChooseFrom, ChooseForOtherPlayer => ProtobufChooseForOtherPlayer, Answer, Choices, Event}

object NetworkPlayerSpec extends Specification with Mockito {

  val mockOtherPlayer = mock[GenericPlayer[Any]]

  def buildInput(mutator: (OutputStream => Unit)) : InputStream = {
    val scratchSpace = new ByteArrayOutputStream(128)
    mutator(scratchSpace)
    new ByteArrayInputStream(scratchSpace)
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

  "network player" should {
    val output = new ByteArrayOutputStream(2048)

    "transmit two choices and retrieve two responses" in {
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

    // FIXME: need some tests that show behaviour with bad streams, protobuf occasionally spits out very poor error messages.
  }
}

