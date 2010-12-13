package org {
  package grumpysoft {
    trait Promptable {
      def prompt(options : Seq[SelfDescribing]) : Int;
    }
  }
}
