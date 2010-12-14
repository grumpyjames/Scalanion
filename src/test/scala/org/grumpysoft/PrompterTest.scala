import scala.collection.mutable.Stack

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

package org {
  package grumpysoft {

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

    class TestPrompterWithFormatter(expectedGreeting: String, formatted: List[String], cannedFormatter: Formatter) extends Promptable with ShouldMatchers{
      def prompt(greeting: SelfDescribing, options: Seq[SelfDescribing]) : Seq[Int] = {
	val stringOptions = options.map(_.describe)
	greeting.describe should equal (expectedGreeting)
	stringOptions should equal (formatted)
	List(Magic.returnValue);
      }

      protected def formatter() : Formatter = { cannedFormatter }
    }

    class FormattedPromptableTest extends WordSpec with ShouldMatchers {
      val unformatted = List(StringDescription("some"), StringDescription("bloody"), StringDescription("strings"))
      val formatted = List("formatted", "strings")
      val greeting = "Hello"

      val cannedFormatter = new Formatter() {
	def format(options: Seq[SelfDescribing]) = {
	  options should equal (unformatted)
	  formatted.map(StringDescription(_))
	}
      }
      val prompter = new TestPrompterWithFormatter(greeting, formatted, cannedFormatter) with FormattedPrompts
      prompter.prompt(StringDescription(greeting), unformatted).head should equal (Magic.returnValue)
    }

    case class StringDescription(val desc: String) extends SelfDescribing {
      def describe() : String = { desc }
    }

    class UniquererTest extends WordSpec with ShouldMatchers {
      uniquery.toSet(List(1,1,1,3,4,5,4,6)) should equal (List(1,3,4,5,6))
    }

    class PrompterTest extends WordSpec with ShouldMatchers {

      val options = List(StringDescription("one"),StringDescription("two"),StringDescription("three"))
      val greeting = "Good day to you, sir!"
      val expectedOutput = List(greeting, "1. one","2. two","3. three")

      def createFixture(inputs: Stack[Seq[Int]]) : (FakeUserInput, FakePrinter, Prompter) = {
	val printer = new FakePrinter()
	val user = new FakeUserInput(inputs)
	(user, printer, new FormattedPrompter(user, printer, new OptionFormatter))
      }

      "a prompter" when {
	"prompted to choose from some options" should {
	  val (user, printer, player) = createFixture(Stack(List(1)))
	  "greet the provided string, then print those options to the supplied printstream" in {
	    val index = player.prompt(StringDescription(greeting), options)
	    assert(printer.printedLines.reverse === expectedOutput)
	    assert(index.head === 1)
	    assert(user.cannedInputs.isEmpty)
	  }
	}
      }      

      "a prompter" when {
	"prompted to choose from some options" should {
	  val (user, printer, player) = createFixture(Stack(List(2),List(5),List(4)))
	  "reject reads that don't match an option, and retry until a valid option is given" in {
	    val index = player.prompt(StringDescription(greeting), options)
	    assert(index.head === 2)
	    assert(printer.printedLines.reverse === (expectedOutput ++ expectedOutput ++ expectedOutput))
	    assert(user.cannedInputs.isEmpty)
	  } 
	}
      }

      def promptExpectingReturn(greeting: String, options: Seq[SelfDescribing], expectedResult: Seq[Int], inputs: Stack[Seq[Int]]) : Unit = {
	val (user, printer, player) = createFixture(inputs)
	val result = player.prompt(StringDescription(greeting), options)
	result should equal (expectedResult)
	user.cannedInputs should be ('empty)
      }

      "a prompter" when {
	"prompted to choose from some options" should {
	  val expectedReturn = List(2,3)

	  "reject reads returning multiple choices if one or more of them is invalid" in {
	    promptExpectingReturn(greeting, options, expectedReturn, Stack(List(2,3), List(2,55), List(0,2), List(5)))
	  }
	}
      }

      "a prompter" when {
	"prompted to choose from some options" should {
	  val expectedReturn = List(2,1)
	  "merge identical results" in {
	    promptExpectingReturn(greeting, options, expectedReturn, Stack(List(2,2,1,1), List(4,5,1), List(2,9)))
	  }	  
	}
      }
    }
  }  
}
