package org.grumpysoft

import scala.collection.mutable.LinearSeq
import scala.collection.mutable.Stack

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec

import TreasureCards._
import ActionCards._
import VictoryCards._

class FakePrompter(var responses : Stack[Seq[Int]]) extends Promptable {
  var received : List[(SelfDescribing, Seq[SelfDescribing])] = Nil
  var messages : List[SelfDescribing] = Nil

  def prompt(greeting: SelfDescribing, options : Seq[SelfDescribing]) : Seq[Int] = {
    received = received ++ List((greeting, options))
    responses.pop
  }

  def prompt(message: SelfDescribing) : Unit = {
    messages = messages ++ List(message)
  }
}

class CommandLinePlayerTest extends WordSpec with ShouldMatchers {

  def checkReceived(description: String, options: Seq[SelfDescribing], prompter: FakePrompter) : Unit = {
    prompter.received.head._1.describe should equal (description)
    prompter.received.head._2 should equal (options)
  }

  def checkReceived(description: String, options: Seq[SelfDescribing], prompter: FakePrompter, count: Int) : Unit = {
    prompter.received.map(a => (a._1.describe, a._2)) should equal (List.fill(4)(description, options))
  }

  def makeTestWith(responses : Stack[Seq[Int]]) : (CommandLinePlayer, FakePrompter) = {
    val prompt = new FakePrompter(responses)
    val player = new CommandLinePlayer("geoff", prompt)
    (player, prompt)
  }

  val threeOptions = List(Remodel(), Copper(), Gold())

  "a command line player" when {
    val (player, prompt) = makeTestWith(Stack(List(2)))
    "asked to choose one card from some cards" should {
      "print the correct instructions, and then return a well formed input" in {
        player.chooseFrom(threeOptions, Discard, 1, 1) should equal (List(2))
        checkReceived("Choose 1 card to discard", threeOptions, prompt)
      }
    }
  }

  "a command line player" when {
    val (player, prompt) = makeTestWith(Stack(List(1,2)))
    "asked to choose two cards from some cards" should {
      "print the correct instructions, and then return a well formed input" in {
        player.chooseFrom(threeOptions, Trash, 2, 2) should equal (List(1,2))
        checkReceived("Choose 2 cards to trash", threeOptions, prompt)
      }
    }
  }

  "a command line player" when {
    val (player, prompt) = makeTestWith(Stack(List()))
    "asked to choose up to one card" should {
      "print the correct instructions, and then return a well formed input" in {
        player.chooseFrom(threeOptions, Play, 0, 1) should equal (List())
        checkReceived("Choose up to 1 card to play", threeOptions, prompt)
      }
    }
  }

  "a command line player" when {
    val (player, prompt) = makeTestWith(Stack(List(1,2,3), List(1,2), List(), List(1,2,3,4)))
    "asked to choose exactly three cards" should {
      "badger the user until they get it right" in {
        player.chooseFrom(threeOptions, Trash, 3, 3) should equal (List(1,2,3))
        checkReceived("Choose 3 cards to trash", threeOptions, prompt, 4)
      }
    }
  }

  "a command line player" when {
    "given a new hand" should {
      "tell the user what the hand contains" in {
        val (player, prompt) = makeTestWith(Stack(List()))
        player.newHand(threeOptions)
        prompt.messages.map(_.describe) should equal (List("Your hand now contains: Remodel, Copper, Gold"))
      }
    }
  }

  "a command line player" when {
    "notified of an event he caused" should {
      "ignore it" in {
        val (player, prompt) = makeTestWith(Stack(List()))
        player.playerEvent(player, Discard, List())
        prompt.messages should be ('empty)
      }
    }
  }

  "a command line player" when {
    "notified of another's events" should {
      "tell the user" in {
        val (player, prompt) = makeTestWith(Stack(List()))
        player.playerEvent(new CommandLinePlayer("Peter", prompt), Discard, threeOptions)
        prompt.messages.map(_.describe) should equal (List("The player Peter discarded: Remodel, Copper, Gold"))
      }
    }
  }


}
