package org.grumpysoft

import scala.collection.mutable.LinearSeq
import scala.collection.mutable.Stack

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec

class FakePrompter(var responses : Stack[Seq[Int]]) extends Promptable {
  var received : List[(SelfDescribing, Seq[SelfDescribing])] = Nil

  def prompt(greeting: SelfDescribing, options : Seq[SelfDescribing]) : Seq[Int] = {
    received = received ++ List((greeting, options))
    responses.pop
  }
}

case class StringCard(val desc: String) extends Card {
  def cost() : Int = { 2 }
  def describe() : String = { desc }
}

class CommandLinePlayerTest extends WordSpec with ShouldMatchers {

  def checkReceived(description: String, options: Seq[SelfDescribing], prompter: FakePrompter) : Unit = {
    prompter.received.head._1.describe should equal (description)
    prompter.received.head._2 should equal (options)
  }

  def makeTestWith(responses : Stack[Seq[Int]]) : (CommandLinePlayer, FakePrompter) = {
    val prompt = new FakePrompter(responses)
    val player = new CommandLinePlayer("geoff", prompt)
    (player, prompt)
  }

  val threeOptions = List(StringCard("Remodel"), StringCard("Copper"), StringCard("Gold"))

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
}
