package org {
  package grumpysoft {
    class Prompter(input: UserInput, output: Printer) extends Promptable {
      def prompt(options : Seq[SelfDescribing]) : Int = {
	readNext(options).dropWhile({response => validReturn(options, response)}).head		
      }

      private def validReturn(options : Seq[SelfDescribing], result : Int) : Boolean = {
	result > options.size || result <= 0
      }

      private def readNext(options: Seq[SelfDescribing]) : Stream[Int] = {
	doPrompt(options)
	Stream.cons(input.read, readNext(options))
      }
      
      private def doPrompt(options: Seq[SelfDescribing]) : Unit = {
	output.println("Choose from:")	
	options.foreach(selfDescribing => output.println(selfDescribing.describe))
      }
    }

    class FormattedPrompter(input: UserInput, output: Printer, optionFormatter: Formatter) extends Prompter(input, output) with FormattedPrompts {
      protected def formatter() : Formatter = {
	optionFormatter
      }
    }
  }
}
