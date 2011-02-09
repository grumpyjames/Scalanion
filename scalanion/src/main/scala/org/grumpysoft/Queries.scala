package org.grumpysoft

sealed abstract class Query(val query: String)

case object DiscardYourDeck extends Query("Do you want to discard your deck?")