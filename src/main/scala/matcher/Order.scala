package matcher

case class Order (
  time: Long,
  orderType: OrderType,
  clientName: String,
  asset: Money[Currency],
  price: Money[Currency]
) {

  val currency: Currency = asset.currency

  def matching(other: Order): Boolean = {
    val priceCond =
      if (orderType == Sell && other.orderType == Buy)
        price == other.price
      else if (orderType == Buy && other.orderType == Sell)
        price == other.price
      else false

      priceCond &&
      asset == other.asset &&
      price.currency == other.price.currency
  }
}

object Order {
  val ordering: Ordering[Order] = Ordering.fromLessThan{ (o1, o2) =>
    (o1, o2) match {
      case (Order(_, orderType1, _, asset1, price1),
            Order(_, orderType2, _, asset2, price2))

        if  orderType1      == orderType2 &&
            asset1.currency == asset2.currency &&
            price1 == price2 =>

          orderType1 match {
            case Sell => price1.value < price2.value
            case Buy  => price1.value >= price2.value
          }

      case _ => o1.time < o2.time
    }
  }
}
