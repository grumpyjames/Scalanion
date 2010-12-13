package org {
  package grumpysoft {
  
    trait Formatter {
      def format(options: Seq[SelfDescribing]) : Seq[SelfDescribing];
    }
  }
}
