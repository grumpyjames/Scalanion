package org {
  package grumpysoft {
    trait Promptable {
      /**
       * Choose some values from a series of options. Indices will be within the desired range,
       * but callers must validate whether the correct number of indices is returned.
       */
      def prompt(greeting: SelfDescribing, options : Seq[SelfDescribing]) : Seq[Int];

      /**
       * notify the player of something that requires no response
       */
      def prompt(message: SelfDescribing) : Unit;
    }
  }
}
