package org {
  package grumpysoft {

    class OptionFormatter extends Formatter {
      def format(options: Seq[String]) : Seq[String] = {
	options.indices.zip(options).map(tuple => (tuple._1 + 1) + ". " + tuple._2)
      }
    }
  }
}
