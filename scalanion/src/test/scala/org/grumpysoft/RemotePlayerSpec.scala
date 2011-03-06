package org.grumpysoft

import org.specs.Specification
import org.grumpysoft.Scalanion.{Event, ServerToClient}
import scalaj.collection.Imports._
import org.specs.mock.Mockito
import org.mockito.Matchers.{eq => meq}
import org.grumpysoft.TreasureCards.{Copper, Silver}
import java.io.{InputStream, OutputStream, ByteArrayInputStream, ByteArrayOutputStream}
import collection.Seq
import java.lang.String

object RemotePlayerSpec extends Specification with Mockito {

  val noOutput = new ByteArrayOutputStream(0)
  val realPlayer = mock[GenericPlayer[Int]]

  def eventInput = {
    val byteArrayOutput = new ByteArrayOutputStream(128)
    ServerToClient.newBuilder.setEvent(Event.newBuilder.setPlayer("thePlayer").setVerb(Gain.present).addAllCard(List("Copper", "Silver").asJava)).build.writeDelimitedTo(byteArrayOutput)
    new ByteArrayInputStream(byteArrayOutput.toByteArray)
  }

  "remote player" should {
    "dispatch the received hand to the wrapped player" in {
      val remotePlayer = RemotePlayer(realPlayer, eventInput, noOutput)
      remotePlayer.readAndForward
      there was one(realPlayer).playerEvent(StringDescription("thePlayer"), Gain, List(Copper(), Silver()))
    }
  }

}

case class RemotePlayer(private val player: GenericPlayer[Int], private val input: InputStream, private val output: OutputStream) {
  def readAndForward : Unit = {
    val message = ServerToClient.parseDelimitedFrom(input)
    val event = message.getEvent
    val seq: Seq[String] = event.getCardList.asScala
    player.playerEvent(liftPlayer(event.getPlayer), liftVerb(event.getVerb), liftCards(seq))
  }

  private def liftPlayer(playerName: String) : SelfDescribing = {
    return StringDescription(playerName)
  }

  private def liftVerb(presentTense: String) : Verb = {
    Gain
  }

  private def liftCards(cards: Seq[String]) : List[Card] = {
    List(Copper(), Silver())
  }
}