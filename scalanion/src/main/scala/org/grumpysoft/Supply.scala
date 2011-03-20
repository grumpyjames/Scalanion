package org.grumpysoft

import util.Random
import org.grumpysoft.VictoryCards.Province

trait Supply {
  def available(card: Card) : Boolean;
  def availableCards(treasure: Int) : Seq[Card];
  def buy(card: Card) : Supply;
  def gameOver() : Boolean;
}

case class SimpleSupply(choices : List[List[Card]]) extends Supply {

  def available(card: Card) : Boolean = {
    choices.exists(_.contains(card))
  }

  def availableCards(treasure: Int) : Seq[Card] = {
    choices.filter(_.size > 0).map(_.head).filter(_.price <= treasure).sortBy(_.price).reverse
  }

  def buy(card: Card) : Supply = {
    // FIXME: evil!
    val (toReduce, unchanged) = choices.partition(_.exists(_.getClass == card.getClass))
    SimpleSupply(toReduce.head.drop(1) :: unchanged.toList)
  }

  def presentable() : String = {
    choices.map(cards => cards.size match {
      case 0 => None
      case a => Some(String.valueOf(a) + " " + cards.head.describe)
    }).filter(_.isDefined).map(_.get).mkString(",")
  }

  override def toString() : String = {
    presentable()
  }

  def gameOver() : Boolean = {
    noProvinces
  }

  def noProvinces() : Boolean = {
    !choices.exists(_.headOption match {
      case Some(card) => card match {
        case p: Province => true
        case _ => false
      }
      case None => false
    })
  }
}

object Supplies {
  private val actionCardCounts = List.fill(10)(10)
  private val victoryCardCounts = List(24, 12, 12)
  private val twoPlayerVictoryCardCounts = List(24, 12, 8)
  private val treasureCardCounts = List(60, 40, 30)

  def multiply(cards: List[Card], counts: List[Int]): List[scala.List[Card]] = {
    cards.zip(counts).map(a => a._1.times(a._2))
  }

  def default() : Supply = {
    SimpleSupply(multiply(Cards.victoryCards, victoryCardCounts) ++ actionsAndTreasure ++ curses)
  }

  def forTwo() : Supply = {
    SimpleSupply(multiply(Cards.victoryCards, twoPlayerVictoryCardCounts) ++ actionsAndTreasure ++ curses)
  }

  private def actionsAndTreasure() : List[List[Card]] = {
    multiply(Random.shuffle(Cards.actionCards).take(10), actionCardCounts) ++
      multiply(Cards.treasureCards, treasureCardCounts)
  }

  private def curses() : List[List[Card]] = {
    List(Curse().times(24))
  }
}
