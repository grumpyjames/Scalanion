package org.grumpysoft

class RichPlayer(player: GenericPlayer[Int]) extends PlayerAdapter[Int](player, (cards: Seq[Card], indices: Seq[Int]) =>
  cards.zip(cards.indices).filter(a => indices.contains(a._2 + 1)).map(_._1))
{
}
