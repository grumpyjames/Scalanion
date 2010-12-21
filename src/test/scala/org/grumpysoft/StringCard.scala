package org.grumpysoft

case class StringCard(val desc: String) extends Card {
  def cost() : Int = { 2 }
  def describe() : String = { desc }
}
