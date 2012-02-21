package org {
  package grumpysoft {

    class OptionFormatter extends Formatter {
      def format(options: Seq[SelfDescribing]) : Seq[SelfDescribing] = {
	options.indices.zip(options).map(tuple => new OrdinalDescriber(tuple._1 + 1, tuple._2))
      }
    }

    class OrdinalDescriber(val index: Int, val wrapped: SelfDescribing) extends SelfDescribing {
      def describe() : String = {
	index + ". " + wrapped.describe
      }
    }
  }
}
