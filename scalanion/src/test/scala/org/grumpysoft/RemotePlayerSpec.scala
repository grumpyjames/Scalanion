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
import org.grumpysoft.Scalanion._

object RemotePlayerSpec extends Specification with Mockito {

  private val noOutput = new ByteArrayOutputStream(0)
  private val realPlayer = mock[GenericPlayer[Int]]
  private val choiceCards: List[ActionCard] = List(Witch(), Remodel(), Remodel())

  def makeInput(streamWriter : (OutputStream => Unit)) = {
    val byteArrayOutput = new ByteArrayOutputStream(128)
    streamWriter(byteArrayOutput)
    inputFrom(byteArrayOutput)
  }

  def inputFrom(baos: ByteArrayOutputStream) : InputStream = {
    new ByteArrayInputStream(baos.toByteArray)
  }

  def eventInput = {
    makeInput(ServerToClient.newBuilder.setEvent(Event.newBuilder.setPlayer("thePlayer").setVerb(Gain.present).addAllCard(List("Copper", "Silver").asJava)).build.writeDelimitedTo(_))
  }

  def handInput = {
    makeInput(ServerToClient.newBuilder.setHand(Hand.newBuilder.addAllCard(List(Copper(), Witch(), Gold(), Copper()).map(_.describe).asJava).build).build.writeDelimitedTo(_))
  }

  def choiceInput = {
    makeInput(ServerToClient.newBuilder.setChooseFrom(ChooseFrom.newBuilder.setVerb(Play.present).setMinimumChoices(2).setMaximumChoices(2).addAllCard(choiceCards.map(_.describe).asJava).build).build.writeDelimitedTo(_))
  }

  val outputBuffer = new ByteArrayOutputStream(128)

  "remote player" should {
    "dispatch the received hand to the wrapped player" in {
      val remotePlayer = RemotePlayer(realPlayer, eventInput, noOutput)
      remotePlayer.readAndForward
      there was one(realPlayer).playerEvent(StringDescription("thePlayer"), Gain, List(Copper(), Silver()))
    }

    "present the player with their new hand" in {
      val remotePlayer = RemotePlayer(realPlayer, handInput, noOutput)
      remotePlayer.readAndForward
      there was one(realPlayer).newHand(List(Copper(), Witch(), Gold(), Copper()))
    }

    "present decisions to the player, and return the correct output" in {
      realPlayer.chooseFrom(choiceCards, Play, 2, 2) returns List(0, 2)
      val remotePlayer = RemotePlayer(realPlayer, choiceInput, outputBuffer)
      remotePlayer.readAndForward
      there was one(realPlayer).chooseFrom(choiceCards, Play, 2, 2)
      Choices.parseDelimitedFrom(inputFrom(outputBuffer)).getChoiceList.asScala must_==(List(0,2))
    }
  }

}

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

  def readAndForward : Unit = {
    val message = ServerToClient.parseDelimitedFrom(input)
    if (message.hasEvent) return passEvent(message)
    if (message.hasHand) return passHand(message)
    if (message.hasChooseFrom) return passChooseFrom(message)
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