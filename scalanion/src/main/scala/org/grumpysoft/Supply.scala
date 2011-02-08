package org.grumpysoft

trait Supply {
  def availableCards(treasure: Int) : Seq[Card];
  def buy(card: Card) : Supply;
}

case class SimpleSupply(choices : List[List[Card]]) extends Supply {
  def availableCards(treasure: Int) : Seq[Card] = {
    choices.filter(_.size > 0).map(_.head)
  }
  def buy(card: Card) : Supply = {
    val (toReduce, unchanged) = choices.partition(_.contains(card))    
    SimpleSupply(toReduce.head.drop(1) :: unchanged.toList)
  }
}
