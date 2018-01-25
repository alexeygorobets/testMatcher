package matcher

import org.specs2.Specification

import scala.collection.SortedSet


class OrderBookSpec extends Specification { def is = s2"""
   Update works correct $updateSpec"""

  implicit val ordering = Order.ordering

  val emptyBook = Map.empty[(Currency, OrderType), SortedSet[Order]]

  val ordersSell1: ((Currency, OrderType), SortedSet[Order]) =
    (A, Sell) -> SortedSet(
    Order(1, Sell, "C1", Money(10, A), Money(2, USD)))

  val ordersBuy2: ((Currency, OrderType), SortedSet[Order]) =
    (A, Buy) -> SortedSet(
    Order(2, Buy, "C2", Money(20, A), Money(1, USD)))

  val bookSell1: Map[(Currency, OrderType), SortedSet[Order]] =
    Map(ordersSell1)

  val bookBuy2: Map[(Currency, OrderType), SortedSet[Order]] =
    Map(ordersBuy2)

  def updateSpec = {
    val orderBuy1 = Order(3, Buy, "C2", Money(10, A), Money(2, USD))
    val orderSell2 = Order(2, Sell, "C1", Money(20, A), Money(1, USD))

    OrderBook(bookSell1).update(orderBuy1)._2 must_== OrderBook(emptyBook)
    OrderBook(bookBuy2).update(orderSell2)._2 must_== OrderBook(emptyBook)
  }
}
