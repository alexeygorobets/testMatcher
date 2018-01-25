package matcher

class Matcher {

  def `match`(balances: Balances, orders: Seq[Order]) = {

    orders.foldLeft((balances, OrderBook.empty)) { case ((prevBalances, orderBook), order) =>
      val (matchingOpt, book) = orderBook.update(order)

      matchingOpt match {
        case Some(matchingOrder) =>
          val newBalances = prevBalances
            .update(order)
            .update(matchingOrder)

          (newBalances, book)
        case None =>
          (prevBalances, book)

      }
    }
  }

}
