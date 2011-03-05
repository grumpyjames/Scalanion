package org.grumpysoft

sealed abstract class Verb(val present: String, val past: String)

// TODO: players need a context in which to place these verbs

case object Play extends Verb("play", "played")
case object Discard extends Verb("discard", "discarded")
case object Buy extends Verb("buy", "bought")
case object Trash extends Verb("trash", "trashed")
case object Gain extends Verb("gain", "gained")
case object Receive extends Verb("receive", "received")
case object Reveal extends Verb("reveal", "revealed")
// TODO: these two are silly
case object PlaceOnDeck extends Verb("decktop", "decktopped")
case object DeckDiscard extends Verb("discard deck", "discarded deck")
case object RevealHand extends Verb("reveal", "revealed")
case object ThiefTrash extends Verb("trash (due to thief)", "trashed (due to thieving)")

