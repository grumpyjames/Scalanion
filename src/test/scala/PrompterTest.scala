import scala.collection.mutable.Stack

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers._

package org {
  package grumpysoft {

    trait Printer {
      def println(s: String) : Unit; 
    }

    trait UserInput {
      def read() : Int;
    }

    class FakePrinter extends Printer {
      var printedLines : List[String] = List()
      def println(s: String) : Unit = { printedLines = s :: printedLines }
    }

    class FakeUserInput(var cannedInputs : Stack[Int]) extends UserInput {
      def read() : Int = {
	cannedInputs.pop
      }
    }

    trait Promptable {
      def prompt(options : Seq[String]) : Unit;
    }

    trait Formatter {
      def format(options: Seq[String]) : Seq[String];
    }

    trait FormattedPrompts extends Promptable {
      abstract override def prompt(options: Seq[String]) : Unit = {
	val formattedOptions = formatter.format(options)
	super.prompt(formattedOptions)
      }

      protected def formatter() : Formatter;
    }

    class PrompterWithFormatter(formatted: List[String], cannedFormatter: Formatter) extends Promptable {
      def prompt(options: Seq[String]) = {
	options should equal (formatted)
      }

      protected def formatter() : Formatter = { cannedFormatter }
    }

    class FormattedPrompter(formatted: List[String], cannedFormatter: Formatter) extends PrompterWithFormatter(formatted, cannedFormatter) with FormattedPrompts {}

    class FormattedPromptableTest extends WordSpec {
      val unformatted = List("some", "bloody", "strings")
      val formatted = List("formatted", "strings")

      val cannedFormatter = new Formatter() {
	def format(options: Seq[String]) = {
	  options should equal (unformatted)
	  formatted
	}
      }
      val prompter = new FormattedPrompter(formatted, cannedFormatter)
      prompter.prompt(unformatted)
    }


    class Prompter(input: UserInput, output: Printer) {
      def prompt(options : Seq[String]) : Int = {
	readNext(options).dropWhile({response => validReturn(options, response)}).head		
      }

      private def validReturn(options : Seq[String], result : Int) : Boolean = {
	result > options.size || result <= 0
      }

      private def readNext(options: Seq[String]) : Stream[Int] = {
	doPrompt(options)
	Stream.cons(input.read, readNext(options))
      }
      
      private def doPrompt(options: Seq[String]) : Unit = {
	output.println("Choose from:")
	options.indices.zip(options).foreach(promptOne(_))
      }

      private def promptOne(optionWithIndex : (Int, String)) : Unit = {
	output.println((optionWithIndex._1 + 1) + ". " + optionWithIndex._2)
      }
    }

    class PrompterTest extends WordSpec {

      val options = List("one","two","three")
      val expectedOutput = List("Choose from:","1. one","2. two","3. three")

      def createFixture(inputs: Stack[Int]) : (FakeUserInput, FakePrinter, Prompter) = {
	val printer = new FakePrinter()
	val user = new FakeUserInput(inputs)
	(user, printer, new Prompter(user, printer))
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
