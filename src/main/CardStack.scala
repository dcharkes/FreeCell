package main

case class CardStack(cards : List[Card]) {
  /*
   * cards: cards in front are on top (should be a singly linked list instead of a list)
   */

  def addCard(card : Card) : Option[CardStack] = {
    if(cards.isEmpty) return Some(CardStack(List(card)))
    val firstCard = cards match { case first :: _ => first }
    if(card.columnFitOn(firstCard)) return Some(CardStack(card :: cards))
    None
  }

  def removeFirst() : Option[CardStack] = cards match {
    case head :: tail => Some(CardStack(tail))
    case _ => None
  }

  def getFirst : Option[Card] = cards match {
    case head :: _ => Some(head)
    case _ => None
  }

  def score : Int = {
    if(cards.isEmpty)
      return 0
    var result = 1
    val it = cards.iterator
    var prev = it.next()
    while(it.hasNext){
      val elem = it.next()
      if(prev.columnFitOn(elem))
        result += 1
    }
    return result
  }
}
