package test

import main.Card

// how do I get Scala unit tests?

object CardTest extends App {
  println(Card(5))
  println(Card(5)==Card(5))
  println(Card(5)==Card(6))

  val hearts5 = Card(5)
  val spades6 = Card(19)
  println(spades6)
  println(spades6.isHearts)
  println(spades6.isSpades)
  println(spades6.getNumber)

  println(spades6)
  println(hearts5.columnFitOn(spades6))
  println(spades6.columnFitOn(hearts5))
}
