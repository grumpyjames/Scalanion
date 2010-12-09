import scala.collection.mutable.Stack

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

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

    class Prompter(input: UserInput, output: Printer) {
      def prompt(options : Seq[String]) : Int = {
	readNext(options).dropWhile({response => validReturn(options, response)}).head		
      }

      private def validReturn(options : Seq[String], result : Int) : Boolean = {
	val valid = result > options.size || result <= 0
	println(result + " was " + valid)
	valid
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

      def createFixture(inputs: Stack[Int]) : (FakeUserInput, FakePrinter, Prompter) = {
	val printer = new FakePrinter()
	val user = new FakeUserInput(inputs)
	(user, printer, new Prompter(user, printer))
      }

      "a command line player" when {
	"prompted to choose from some options" should {
	  val (user, printer, player) = createFixture(Stack(1))
	  "ask the provided stream to choose, then print those options to the supplied printstream" in {
	    val index = player.prompt(List("one","two","three"))
	    assert(printer.printedLines.reverse === List("Choose from:","1. one", "2. two", "3. three"))
	    assert(index === 1)
	    assert(user.cannedInputs.isEmpty)
	  }
	}
      }      

      val expectedOutput = List("Choose from:","1. one","2. two")

      "a command line player" when {
	"prompted to choose from some options" should {
	  val (user, printer, player) = createFixture(Stack(2,3,4))
	  "reject reads that don't match an option, and retry until a valid option is given" in {
	    val index = player.prompt(List("one","two"))
	    assert(printer.printedLines.reverse === (expectedOutput ++ expectedOutput ++ expectedOutput))
	    assert(index === 2)
	    assert(user.cannedInputs.isEmpty)
	  } 
	}
      }
    }
  }  
}
