package main

case class Card(number: Int) {

  override def toString: String = number match {
    case 1 => "HA" //red
    case 2 => "H2"
    case 3 => "H3"
    case 4 => "H4"
    case 5 => "H5"
    case 6 => "H6"
    case 7 => "H7"
    case 8 => "H8"
    case 9 => "H9"
    case 10 => "HX"
    case 11 => "HJ"
    case 12 => "HQ"
    case 13 => "HK"
    case 14 => "SA" //black
    case 15 => "S2"
    case 16 => "S3"
    case 17 => "S4"
    case 18 => "S5"
    case 19 => "S6"
    case 20 => "S7"
    case 21 => "S8"
    case 22 => "S9"
    case 23 => "SX"
    case 24 => "SJ"
    case 25 => "SQ"
    case 26 => "SK"
    case 27 => "DA" //red
    case 28 => "D2"
    case 29 => "D3"
    case 30 => "D4"
    case 31 => "D5"
    case 32 => "D6"
    case 33 => "D7"
    case 34 => "D8"
    case 35 => "D9"
    case 36 => "DX"
    case 37 => "DJ"
    case 38 => "DQ"
    case 39 => "DK"
    case 40 => "CA" //black
    case 41 => "C2"
    case 42 => "C3"
    case 43 => "C4"
    case 44 => "C5"
    case 45 => "C6"
    case 46 => "C7"
    case 47 => "C8"
    case 48 => "C9"
    case 49 => "CX"
    case 50 => "CJ"
    case 51 => "CQ"
    case 52 => "CK"
    case _ => "??"
  }

  def isHearts : Boolean = number >= 1 && number <= 13
  def isSpades : Boolean = number >= 14 && number <= 26
  def isDiamonds : Boolean = number >= 27 && number <= 39
  def isClubs : Boolean = number >= 40 && number <= 52

  def isRed = isHearts || isDiamonds
  def isBlack = isSpades || isClubs

  def getNumber : Int = {
    if(isHearts) return number
    if(isSpades) return number - 13
    if(isDiamonds) return number - 26
    return number - 39
  }

  def columnFitOn(other:Card) = {
    val diffColor = isRed && other.isBlack || isBlack && other.isRed
    val diffNumber = other.getNumber - getNumber
    diffNumber == 1 && diffColor
  }

}

object Card{
  def apply(text:String) : Card = {
      val suit = text.charAt(0) match {
        case 'H' => 0
        case 'S' => 13
        case 'D' => 26
        case 'C' => 39
      }
      val number = text.charAt(1) match {
        case 'A' => 1
        case 'X' => 10
        case 'J' => 11
        case 'Q' => 12
        case 'K' => 13
        case _ => Integer.parseInt(text.charAt(1).toString)
      }
      Card(suit+number)
  }
}