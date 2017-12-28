package test

import main.Card
import main.CardStack
import main.State

object StateTest extends App {
  val emptyState = State(Set(), None, None, None, None, Set(), None, 0)
  println(emptyState)

  val state1 = State(Set(Card(15)), Some(Card(12)), None, None, None, Set(CardStack(List(Card(11), Card(10))),CardStack(List(Card(9), Card(7)))), None, 0)
  val state1eq = State(Set(Card(15)), Some(Card(12)), None, None, None, Set(CardStack(List(Card(11), Card(10))),CardStack(List(Card(9), Card(7)))), None, 0)
  println(state1)
  println(state1==state1)
  println(state1==state1eq)

  val state2 = State(Set(Card(15)), None, None, None, None, Set(CardStack(List(Card(11), Card(10))),CardStack(List(Card(9), Card(7)))), None, 0)
  println(state1==state2)

  val stateMoveToFoundation = State(Set(Card(2)), Some(Card(1)), Some(Card(14)), None, None, Set(CardStack(List(Card(15)))), None, 0)
  println("input")
  println(stateMoveToFoundation)
  println("nextStates")
  stateMoveToFoundation.nextStates.map(println)
}
