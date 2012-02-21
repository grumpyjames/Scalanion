package org.grumpysoft

case class StringDescription(val desc: String) extends SelfDescribing {
  def describe() : String = { desc }
}
