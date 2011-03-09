package org.grumpysoft

sealed abstract class Query

case class BasicQuestion(query: String) extends Query
case class ChooseForOtherPlayer(cards: Seq[Card], otherPlayer: SelfDescribing, verb: Verb) extends Query