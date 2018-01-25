package matcher

import scala.collection.SortedSet

case class OrderBook(private val book: Map[(Currency, OrderType), SortedSet[Order]]) {
  implicit val _ = Order.ordering

  def update(order: Order): (Option[Order], OrderBook) = {
    val key: (Currency, OrderType) = order.currency -> order.orderType.opposite

    book.get(key) match {
      case Some(orders) =>
        orders.find(order.matching) match {
          case Some(matchingOrder) =>
            (Some(matchingOrder), remove(key, orders, matchingOrder))
          case None =>
            (None, put(order))
        }
      case None =>
        (None, OrderBook(book + (key -> SortedSet(order))))
    }
  }

  private def remove(key: (Currency, OrderType), orders: SortedSet[Order], order: Order): OrderBook = {
    val newSet = orders.dropWhile(_ == order)
    if (newSet.nonEmpty) {
      OrderBook(book + (key -> newSet))
    } else {
      OrderBook(book - key)
    }
  }

  private def put(order: Order): OrderBook = {
    val key = order.currency -> order.orderType
    book.get(key) match {
      case Some(orders) =>
        OrderBook(book + (key -> (orders + order)))
      case None =>
        OrderBook(book + (key -> SortedSet(order)))
    }
  }

}

object OrderBook {
  def empty: OrderBook = OrderBook(Map.empty)
}