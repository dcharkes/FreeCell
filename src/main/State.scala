package main

case class State(
                val free : Set[Card], // at most 4
                val foundationH : Option[Card],
                val foundationS : Option[Card],
                val foundationD : Option[Card],
                val foundationC : Option[Card],
                val columns : Set[CardStack] // at most 8
                ) {

  override def toString: String =
    s"""free       : $free
       |foundation : $foundationH, $foundationS, $foundationD, $foundationC
       |stacks     : ${columns.map("\n                 " + _.toString)}
     """.stripMargin
}
