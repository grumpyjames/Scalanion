package org.grumpysoft

import actioncards.Remodel
import scala.collection.mutable.Stack

import TreasureCards._
import org.specs.Specification

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

class CommandLinePlayerSpec extends Specification {

  def checkReceived(description: String, options: Seq[SelfDescribing], prompter: FakePrompter) : Unit = {
    prompter.received.head._1.describe must_==description
    prompter.received.head._2 must_==options
  }

  def checkReceived(description: String, options: Seq[SelfDescribing], prompter: FakePrompter, count: Int) : Unit = {
    prompter.received.map(a => (a._1.describe, a._2)) must_==List.fill(4)(description, options)
  }

  def makeTestWith(responses : Stack[Seq[Int]]) : (CommandLinePlayer, FakePrompter) = {
    val prompt = new FakePrompter(responses)
    val player = new CommandLinePlayer("geoff", prompt)
    (player, prompt)
  }

  val threeOptions = List(Remodel(), Copper(), Gold())

  "a command line player" should {
    "print the correct instructions, and then return a well formed input, for precisely one card" in {
      val (player, prompt) = makeTestWith(Stack(List(2)))
      player.chooseFrom(threeOptions, Discard, 1, 1) must_== (List(2))
      checkReceived("Choose 1 card to discard", threeOptions, prompt)
    }

    "print the correct instructions, and then return a well formed input, for precisely two cards" in {
      val (player, prompt) = makeTestWith(Stack(List(1,2)))
      player.chooseFrom(threeOptions, Trash, 2, 2) must_== (List(1,2))
      checkReceived("Choose 2 cards to trash", threeOptions, prompt)
    }

    "print the correct instructions, and then return a well formed input, for either zero or one cards" in {
      val (player, prompt) = makeTestWith(Stack(List()))
      player.chooseFrom(threeOptions, Play, 0, 1) must_== (List())
      checkReceived("Choose up to 1 card to play", threeOptions, prompt)
    }

    "badger the user until they select exactly the right number of cards" in {
      val (player, prompt) = makeTestWith(Stack(List(1,2,3), List(1,2), List(), List(1,2,3,4)))
      player.chooseFrom(threeOptions, Trash, 3, 3) must_== (List(1,2,3))
      checkReceived("Choose 3 cards to trash", threeOptions, prompt, 4)
    }

    "tell the user what their hand contains" in {
      val (player, prompt) = makeTestWith(Stack(List()))
      player.newHand(threeOptions)
      prompt.messages.map(_.describe) must_== List("Your hand now contains: Remodel, Copper, Gold")
    }

    "ignored events generated by herself" in {
      val (player, prompt) = makeTestWith(Stack(List()))
      player.playerEvent(player, Discard, List())
      prompt.messages must_==Nil
    }


    "tell the user about events" in {
      val (player, prompt) = makeTestWith(Stack(List()))
      player.playerEvent(new CommandLinePlayer("Peter", prompt), Discard, threeOptions)
      prompt.messages.map(_.describe) must_== List("The player Peter discarded: Remodel, Copper, Gold")
    }

    "tell the user about the game starting" in {
      val (player, prompt) = makeTestWith(Stack(List()))
      val startTime = System.currentTimeMillis
      player.gameEvent(Start(startTime))
      prompt.messages.map(_.describe) must_==List("The game begins!")
    }

    "tell the user about the game ending" in {
      val (player, prompt) = makeTestWith(Stack(List()))
      val leaderboard = List((StringDescription("foo"), 12), (StringDescription("bar"), 11))
      player.gameEvent(End(leaderboard))
      prompt.messages.map(_.describe) must_==List("The game ended. Scores were:", "1.) foo : 12", "2.) bar : 11")
    }

  }


}
