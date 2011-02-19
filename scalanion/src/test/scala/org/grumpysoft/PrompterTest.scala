package org.grumpysoft

import scala.collection.mutable.Stack

import org.specs.Specification

class FakePrinter extends Printer {
  var printedLines : List[String] = List()
  def println(s: String) : Unit = { printedLines = s :: printedLines }
}

class FakeUserInput(var cannedInputs : Stack[Seq[Int]]) extends UserInput {
  def read() : Seq[Int] = {
    cannedInputs.pop
  }
}

object Magic {
  val returnValue = 5;
}

trait DevNullPrompter extends Promptable {
  def prompt(message: SelfDescribing) : Unit = {}
}

object FormattedPromptableSpec extends Specification {
  class TestPrompterWithFormatter(expectedGreeting: String, formatted: List[String], cannedFormatter: Formatter) extends DevNullPrompter {
    def prompt(greeting: SelfDescribing, options: Seq[SelfDescribing]) : Seq[Int] = {
      val stringOptions = options.map(_.describe)
      greeting.describe must_==expectedGreeting
      stringOptions must_==formatted
      List(Magic.returnValue);
    }

    protected def formatter() : Formatter = { cannedFormatter }
  }

  val unformatted = List(StringDescription("some"), StringDescription("bloody"), StringDescription("strings"))
  val formatted = List("formatted", "strings")
  val greeting = "Hello"

  val cannedFormatter = new Formatter() {
    def format(options: Seq[SelfDescribing]) = {
      options must_==unformatted
      formatted.map(StringDescription(_))
    }
  }
  val prompter = new TestPrompterWithFormatter(greeting, formatted, cannedFormatter) with FormattedPrompts
  prompter.prompt(StringDescription(greeting), unformatted).head must_== Magic.returnValue
}

class UniquererSpec extends Specification {
  "uniquerer" should {
    "settify a sequence" in {
      uniquery.toSet(List(1,1,1,3,4,5,4,6)) must_==List(1,3,4,5,6)
    }
  }
}

class PrompterSpec extends Specification {

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
      val (user, printer, player) = createFixture(Stack(List(2),List(5),List(4)))
      val index = player.prompt(StringDescription(greeting), options)
      index.head must_== 2
      printer.printedLines.reverse must_== expectedOutput ++ expectedOutput ++ expectedOutput
      user.cannedInputs.isEmpty must_== true
    }

    "reject reads returning multiple choices if one or more of them is invalid" in {
      val expectedReturn = List(2,3)
      promptExpectingReturn(greeting, options, expectedReturn, Stack(List(2,3), List(2,55), List(0,2), List(5)))
    }

    "merge identical results" in {
      val expectedReturn = List(2,1)
      promptExpectingReturn(greeting, options, expectedReturn, Stack(List(2,2,1,1), List(4,5,1), List(2,9)))
    }

    "just print update messages verbatim" in {
      expectPromptOf("hello\nworld")
    }
  }

  def promptExpectingReturn(greeting: String, options: Seq[SelfDescribing], expectedResult: Seq[Int], inputs: Stack[Seq[Int]]) : Unit = {
    val (user, printer, player) = createFixture(inputs)
    val result = player.prompt(StringDescription(greeting), options)
    result must_==expectedResult
    user.cannedInputs.isEmpty must_==true
  }

  def expectPromptOf(message: String) : Unit = {
    val (user, printer, player) = createFixture(Stack(Nil))
    player.prompt(StringDescription(message))
    printer.printedLines must_==List(message)
  }

  def createFixture(inputs: Stack[Seq[Int]]) : (FakeUserInput, FakePrinter, Prompter) = {
    val printer = new FakePrinter()
    val user = new FakeUserInput(inputs)
    (user, printer, new FormattedPrompter(user, printer, new OptionFormatter))
  }
}
