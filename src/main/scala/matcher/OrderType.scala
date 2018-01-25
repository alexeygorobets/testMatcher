package matcher

sealed trait OrderType {
  def opposite: OrderType = this match {
    case Sell => Buy
    case Buy => Sell
  }
}

case object Sell extends OrderType
case object Buy extends OrderType

object OrderType {
  def apply(s: String): OrderType = s match {
    case "s" => Sell
    case "b" => Buy
  }
}