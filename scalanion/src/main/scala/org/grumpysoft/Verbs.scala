package org.grumpysoft

sealed abstract class Verb(val present: String, val past: String)

case object Play extends Verb("play", "played")
case object Discard extends Verb("discard", "discarded")
case object Buy extends Verb("buy", "bought")
case object Trash extends Verb("trash", "trashed")
case object Receive extends Verb("receive", "received")
// TODO: this is silly
case object PlaceOnDeck extends Verb("decktop", "decktopped")

