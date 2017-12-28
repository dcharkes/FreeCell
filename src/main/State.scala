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
//        result ++= (state.addColumns(card, this) - this) // disabling this gives a 400 moves solution!? (every move needs to go through a free space now)
      }
    }
    result
  }

  def score : Int = {
    (foundationH++foundationC++foundationS++foundationD).map(_.getNumber).sum
  }

  def score2 : Int = 10 * score - free.size

  def score3 : Int = 10000 * score + 1000 * (8 - columns.size) + 10 * columns.map(_.score).sum - free.size // doesnt work, finds a 5000 moves solution with dfs

  def stateSequence : List[State]  = {
    var elem = this
    var states = List(elem)
    while(elem.previous.nonEmpty){
      elem = elem.previous.get
      states = elem :: states
    }
    states
  }

  def cardMoved : String = {
    if(previous.nonEmpty){
      val newFree: Set[Card] = free -- previous.get.free
      if(newFree.nonEmpty)
        return "" + newFree.iterator.next() + " to free"
      if(foundationH != previous.get.foundationH)
        return "" + foundationH.get + " to foundation"
      if(foundationS != previous.get.foundationS)
        return "" + foundationS.get + " to foundation"
      if(foundationD != previous.get.foundationD)
        return "" + foundationD.get + " to foundation"
      if(foundationC != previous.get.foundationC)
        return "" + foundationC.get + " to foundation"
      val editedColumns = columns -- previous.get.columns
      val editedColumn = editedColumns.filter(_.cards.nonEmpty).filter(c => previous.get.columns.contains(c.removeFirst().get) || c.cards.size == 1)
      if(editedColumn.nonEmpty) {
        val column = editedColumn.iterator.next()
        val card = column.getFirst.get
        val onTopOf = column.removeFirst().flatMap(_.getFirst)
        if(onTopOf.nonEmpty)
          return "" + editedColumn.iterator.next().getFirst.get + " to " + onTopOf.get
        else
          return "" + editedColumn.iterator.next().getFirst.get + " to empty column"
      }
    }
    "?"
  }
}
