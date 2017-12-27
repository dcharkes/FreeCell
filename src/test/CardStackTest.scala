package test

import main.Card
import main.CardStack

object CardStackTest extends App {
  println(CardStack(List(Card(5),Card(6))))
  println(CardStack(List(Card(5),Card(6)))==CardStack(List(Card(5),Card(6))))
  println(CardStack(List(Card(5),Card(6)))==CardStack(List(Card(7),Card(6))))
}
