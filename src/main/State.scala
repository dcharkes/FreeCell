package main

case class State(
                free : Set[Card], // at most 4
                foundationH : Option[Card],
                foundationS : Option[Card],
                foundationD : Option[Card],
                foundationC : Option[Card],
                columns : Set[CardStack], // at most 8
                previous : Option[State],
                moves : Int
                ) {

  override def toString: String =
    s"""free       : $free
       |foundation : $foundationH, $foundationS, $foundationD, $foundationC
       |stacks     : ${columns.map("\n                 " + _.toString)}
     """.stripMargin

  override def equals(obj: scala.Any): Boolean = obj match {
    case s@State(_,_,_,_,_,_,_,_) => free == s.free && foundationH == s.foundationH && foundationS == s.foundationS && foundationD == s.foundationD && foundationC == s.foundationC && columns == s.columns
  }

  override def hashCode(): Int = free.hashCode() ^ foundationH.hashCode() ^ foundationS.hashCode() ^ foundationD.hashCode() ^ foundationC.hashCode() ^ columns.hashCode()

  /**
    * does not remove the card from the previous place
    * @param card
    * @return
    */
  def addFoundation(card : Card, previous : State) : Option[State] = {
    if(card.isHearts && (card.getNumber == 1 && foundationH.isEmpty || foundationH.map(_.getNumber).contains(card.getNumber - 1))) return Some(State(free, Some(card), foundationS, foundationD, foundationC, columns, Some(previous), previous.moves+1))
    if(card.isSpades && (card.getNumber == 1 && foundationS.isEmpty || foundationS.map(_.getNumber).contains(card.getNumber - 1))) return Some(State(free, foundationH, Some(card), foundationD, foundationC, columns, Some(previous), previous.moves+1))
    if(card.isDiamonds && (card.getNumber == 1 && foundationD.isEmpty || foundationD.map(_.getNumber).contains(card.getNumber - 1))) return Some(State(free, foundationH, foundationS, Some(card), foundationC, columns, Some(previous), previous.moves+1))
    if(card.isClubs && (card.getNumber == 1 && foundationC.isEmpty || foundationC.map(_.getNumber).contains(card.getNumber - 1))) return Some(State(free, foundationH, foundationS, foundationD, Some(card), columns, Some(previous), previous.moves+1))
    None
  }

  def addFree(card : Card, previous : State) : Option[State] = {
    if(free.size < 4) return Some(State(free+card, foundationH, foundationS, foundationD, foundationC, columns, Some(previous), previous.moves+1))
    None
  }

  def addColumns(card : Card, previous : State) : Set[State] = {
    val newColumns = columns.flatMap(_.addCard(card))
    var result = newColumns.map { column =>
      val newCols = columns-column.removeFirst().get + column
      State(free, foundationH, foundationS, foundationD, foundationC, newCols, Some(previous), previous.moves+1)
    }
    if(columns.size < 8){
      result += State(free, foundationH, foundationS, foundationD, foundationC, columns + CardStack(List(card)), Some(previous), previous.moves+1)
    }
    result
  }

  def nextStates : Set[State] = {
    var result : Set[State] = Set()
    for (elem <- free) {
      val state = State(free - elem, foundationH, foundationS, foundationD, foundationC, columns, Some(this), this.moves+1)
      result ++= state.addFoundation(elem, this)
      result ++= state.addColumns(elem, this)
    }
    for (column <- columns) {
      if(column.cards.nonEmpty) {
        val card = column.getFirst.get
        val newColumn = column.removeFirst().get
        val state = State(free, foundationH, foundationS, foundationD, foundationC, (columns - column + newColumn).filter(_.cards.nonEmpty), Some(this), this.moves+1)
        result ++= state.addFree(card, this)
        result ++= state.addFoundation(card, this)
        result ++= (state.addColumns(card, this) - this)
      }
    }
    result
  }

  def score : Int = {
    (foundationH++foundationC++foundationS++foundationD).map(_.getNumber).sum
  }
}
