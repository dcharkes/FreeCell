package main

import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet

object DepthFirstSearch {

  def dfs(startState : State) : State = {

    val seen : HashSet[State] = HashSet()
    seen.add(startState)

    val stack : Stack[State] = Stack()
    stack.push(startState)

    var allStates = 0
    var highestScore = 0
    var highestScoreRound = 0
    var shortestPathRound = Int.MaxValue
    var longestPathRound = 0

    while(stack.nonEmpty){
      val nextStates = stack.pop().nextStates.toList.sortBy(_.score2) //finds 1100 moves solution after 1.1 mil states (inverse score finds 6000 moves solution almost immediately)
      for(elem <- nextStates){
        allStates += 1
        if(!seen.contains(elem)){
          seen.add(elem)
          stack.push(elem)
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
          return elem
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

    null
  }

}
