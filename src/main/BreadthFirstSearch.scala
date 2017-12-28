package main

import scala.collection.mutable.{HashSet, Queue}

object BreadthFirstSearch {



  def bfs(startState : State) = {

    val seen : HashSet[State] = HashSet()
    seen.add(startState)

    val stack : Queue[State] = Queue()
    stack.enqueue(startState)

    var allStates = 0
    var highestScore = 0
    var highestScoreRound = 0
    var shortestPathRound = Int.MaxValue
    var longestPathRound = 0

    while(stack.nonEmpty){
      val nextStates = stack.dequeue().nextStates
      for(elem <- nextStates){
        allStates += 1
        if(!seen.contains(elem)){
          seen.add(elem)
          stack.enqueue(elem)
          if(elem.score > highestScore)
            highestScore = elem.score
          if(elem.score > highestScoreRound)
            highestScoreRound = elem.score
        }
        if(elem.score == 52){
          var pathLength = 0
          var el = elem
          while(el.previous.nonEmpty){
            el = el.previous.get
            pathLength += 1
          }
          println(pathLength)
        }
        if(elem.moves < shortestPathRound)
          shortestPathRound = elem.moves
        if(elem.moves > longestPathRound)
          longestPathRound = elem.moves
        if(allStates%100000==0){
          println(allStates + " " + seen.size + " " + stack.size + " " + highestScore + " " + highestScoreRound + " " + shortestPathRound + " " + longestPathRound)
          highestScoreRound = 0
          shortestPathRound = Int.MaxValue
          longestPathRound = 0
        }
      }
    }

    seen
  }

}
