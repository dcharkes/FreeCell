package test

import main.Card
import main.CardStack
import main.State

object StateTest extends App {
  val emptyState = State(Set(), None, None, None, None, Set())
  println(emptyState)

  val state1 = State(Set(Card(15)), Some(Card(12)), None, None, None, Set(CardStack(List(Card(11), Card(10))),CardStack(List(Card(9), Card(7)))))
  println(state1)
  println(state1==state1)

  val state2 = State(Set(Card(15)), None, None, None, None, Set(CardStack(List(Card(11), Card(10))),CardStack(List(Card(9), Card(7)))))
  println(state1==state2)
}
