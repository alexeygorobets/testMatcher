package matcher

import org.specs2.Specification

import scala.collection.SortedSet

class OrderSpec extends Specification { def is =    s2"""
   Order ops works correct $orderSpec
   Order ordering works correct $orderOrderingSpec
  """

  val orderSell1 = Order(1, Sell, "C1", Money(10, A), Money(2, USD))
  val orderBuy1 = Order(2, Buy, "C2", Money(10, A), Money(2, USD))
  val orderSell2 = Order(3, Sell, "C1", Money(10, A), Money(3, USD))
  val orderBuy2 = Order(4, Buy, "C2", Money(10, A), Money(2, USD))

  def orderSpec = {
    orderBuy1.matching(orderBuy1) must_== true
    orderBuy1.matching(orderBuy1) must_== true

    orderSell2.matching(orderBuy1) must_== false
    orderBuy1.matching(orderSell2) must_== false
  }

  def orderOrderingSpec = {
    implicit val _ = Order.ordering
    SortedSet(orderSell2, orderSell1).toList must_== List(orderSell1, orderSell2)
    SortedSet(orderBuy2, orderBuy1).toList must_== List(orderBuy1, orderBuy2)
  }

}


