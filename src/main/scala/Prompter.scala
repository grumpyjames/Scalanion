package org {
  package grumpysoft {
    class Prompter(input: UserInput, output: Printer) extends Promptable {
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
	options.foreach(output.println(_))
      }
    }

    class FormattedPrompter(input: UserInput, output: Printer, optionFormatter: Formatter) extends Prompter(input, output) with FormattedPrompts {
      protected def formatter() : Formatter = {
	optionFormatter
      }
    }
  }
}
