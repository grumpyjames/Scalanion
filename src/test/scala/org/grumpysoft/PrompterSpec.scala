package org.grumpysoft

import scala.collection.mutable.Stack

import org.specs2.mutable.Specification

object PrompterSpec extends Specification {

  class FakePrinter extends Printer {
    var printedLines : List[String] = List()
    def println(s: String) { printedLines = s :: printedLines }
  }

  class FakeUserInput(var cannedInputs : Stack[Seq[Int]]) extends UserInput {
    def read() : Seq[Int] = {
      cannedInputs.pop()
    }
  }

  val options = List(StringDescription("one"),StringDescription("two"),StringDescription("three"))
  val greeting = "Good day to you, sir!"
  val expectedOutput = List(greeting, "1. one","2. two","3. three")


  "a prompter" should {
    "print the provided string, then those options to the supplied printstream, when prompted to choose from some options" in {
      val (user, printer, player) = createFixture(Stack(List(1)))
      val index = player.prompt(StringDescription(greeting), options)
      printer.printedLines.reverse must_==expectedOutput
      index.head must_==1
      user.cannedInputs.size must_==0
    }

    "reject reads that don't match an option, and retry until a valid option is given" in {
      val (user, printer, player) = createFixture(Stack(List(4),List(5),List(2)))
      val index = player.prompt(StringDescription(greeting), options)
      (index.head must_== 2) and
        (printer.printedLines.reverse must_== (expectedOutput ++ expectedOutput ++ expectedOutput)) and
        (user.cannedInputs.isEmpty must_== true)
    }

    "reject reads returning multiple choices if one or more of them is invalid" in {
      val expectedReturn = List(2,3)
      promptExpectingReturn(greeting, options, expectedReturn, Stack(List(2,55), List(0,2), List(5), List(2,3)))
    }

    "merge identical results" in {
      val expectedReturn = List(2,1)
      promptExpectingReturn(greeting, options, expectedReturn, Stack(List(4,5,1), List(2,9), List(2,2,1,1)))
    }

    "just print update messages verbatim" in {
      expectPromptOf("hello\nworld")
    }
  }

  def promptExpectingReturn(greeting: String, options: Seq[SelfDescribing], expectedResult: Seq[Int], inputs: Stack[Seq[Int]]) = {
    val (user, _, player) = createFixture(inputs)
    val result = player.prompt(StringDescription(greeting), options)
    (result must_==expectedResult) and (user.cannedInputs must beEmpty)
  }

  def expectPromptOf(message: String) = {
    val (_, printer, player) = createFixture(Stack(Nil))
    player.prompt(StringDescription(message))
    printer.printedLines must_==List(message)
  }

  def createFixture(inputs: Stack[Seq[Int]]) : (FakeUserInput, FakePrinter, Prompter) = {
    val printer = new FakePrinter()
    val user = new FakeUserInput(inputs)
    (user, printer, new FormattedPrompter(user, printer, new OptionFormatter))
  }
}
