package org {
  package grumpysoft {

     trait FormattedPrompts extends Promptable {
      abstract override def prompt(options: Seq[String]) : Int = {
	val formattedOptions = formatter.format(options)
	super.prompt(formattedOptions)
      }

      protected def formatter() : Formatter;
    }

 }
}
