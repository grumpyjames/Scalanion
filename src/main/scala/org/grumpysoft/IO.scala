package org {
  package grumpysoft {
    trait Printer {
      def println(s: String) : Unit; 
    }

    trait UserInput {
      def read() : Seq[Int];
    }
  }
}
