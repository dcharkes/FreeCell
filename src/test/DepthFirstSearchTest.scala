package test

import main.{Card, CardStack, DepthFirstSearch, State}

object DepthFirstSearchTest extends App {

//  val stateMoveToFoundation = State(Set(Card(2)), Some(Card(1)), Some(Card(14)), None, None, Set(CardStack(List(Card(15)))), None)

//  DepthFirstSearch.dfs(stateMoveToFoundation).map(println)

  val state617 = State(Set(), None, None, None, None, Set(
    CardStack(List(Card("DJ"),Card("C4"),Card("S4"),Card("DK"),Card("HX"),Card("DX"),Card("D7"))),
    CardStack(List(Card("CK"),Card("CX"),Card("CQ"),Card("H5"),Card("SQ"),Card("C7"),Card("DA"))),
    CardStack(List(Card("SK"),Card("H2"),Card("S9"),Card("C9"),Card("H3"),Card("DQ"),Card("S5"))),
    CardStack(List(Card("H4"),Card("D5"),Card("H9"),Card("S3"),Card("D9"),Card("SA"),Card("C3"))),
    CardStack(List(Card("SJ"),Card("S7"),Card("C8"),Card("C6"),Card("D6"),Card("C5"))),
    CardStack(List(Card("S6"),Card("H6"),Card("H7"),Card("D8"),Card("H8"),Card("S8"))),
    CardStack(List(Card("HJ"),Card("S2"),Card("D4"),Card("D3"),Card("CA"),Card("D2"))),
    CardStack(List(Card("HQ"),Card("C2"),Card("CJ"),Card("SX"),Card("HK"),Card("HA")))
  ),None,0)

  println(state617)

//  state617.nextStates.map(println)

  // finds solution in 1000000 iterations
  val endState = DepthFirstSearch.dfs(state617)
  val stateSequence = endState.stateSequence
//  val moves = stateSequence.map(_.cardMoved)
  var i = 0
  for(elem <- stateSequence){
    println(elem.cardMoved)
    if(i%10==0) {
      println(elem.moves)
      println(elem)
    }
    i += 1
  }
//  moves.map(println)
}
