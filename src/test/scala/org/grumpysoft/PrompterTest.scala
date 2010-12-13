import scala.collection.mutable.Stack

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers._

package org {
  package grumpysoft {

    class FakePrinter extends Printer {
      var printedLines : List[String] = List()
      def println(s: String) : Unit = { printedLines = s :: printedLines }
    }

    class FakeUserInput(var cannedInputs : Stack[Int]) extends UserInput {
      def read() : Int = {
	cannedInputs.pop
      }
    }

    object Magic {
      val returnValue = 5;
    }

    class PrompterWithFormatter(formatted: List[String], cannedFormatter: Formatter) extends Promptable {
      def prompt(options: Seq[String]) : Int = {
	options should equal (formatted)
	Magic.returnValue;
      }

      protected def formatter() : Formatter = { cannedFormatter }
    }

    class FormattedPromptableTest extends WordSpec {
      val unformatted = List("some", "bloody", "strings")
      val formatted = List("formatted", "strings")

      val cannedFormatter = new Formatter() {
	def format(options: Seq[String]) = {
	  options should equal (unformatted)
	  formatted
	}
      }
      val prompter = new PrompterWithFormatter(formatted, cannedFormatter) with FormattedPrompts
      prompter.prompt(unformatted) should equal (Magic.returnValue)
    }

    class OptionFormatterTest extends WordSpec {
      val unformatted = List("forum","quorum","no")
      "an option formatter" when {
	"asked to format some options" should {
	  "number and format them nicely" in {
	    val formatted = new OptionFormatter().format(unformatted)
	    formatted should equal (List("1. forum", "2. quorum", "3. no"))	    
	  }
	}
      }
    }

    class PrompterTest extends WordSpec {

      val options = List("one","two","three")
      val expectedOutput = List("Choose from:","1. one","2. two","3. three")

      def createFixture(inputs: Stack[Int]) : (FakeUserInput, FakePrinter, Prompter) = {
	val printer = new FakePrinter()
	val user = new FakeUserInput(inputs)
	(user, printer, new FormattedPrompter(user, printer, new OptionFormatter))
      }

      "a command line player" when {
	"prompted to choose from some options" should {
	  val (user, printer, player) = createFixture(Stack(1))
	  "ask the provided stream to choose, then print those options to the supplied printstream" in {
	    val index = player.prompt(options)
	    assert(printer.printedLines.reverse === expectedOutput)
	    assert(index === 1)
	    assert(user.cannedInputs.isEmpty)
	  }
	}
      }      

      "a command line player" when {
	"prompted to choose from some options" should {
	  val (user, printer, player) = createFixture(Stack(2,5,4))
	  "reject reads that don't match an option, and retry until a valid option is given" in {
	    val index = player.prompt(options)
	    assert(index === 2)
	    assert(printer.printedLines.reverse === (expectedOutput ++ expectedOutput ++ expectedOutput))
	    assert(user.cannedInputs.isEmpty)
	  } 
	}
      }

    }
  }  
}
