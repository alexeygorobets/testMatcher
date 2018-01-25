package matcher

import scala.io.Source

object Parsing {
  def getClientBalancesFromFile(clientsFile: String) = {
    getClientBalancesFromLines(Source.fromResource(clientsFile).getLines.toSeq)
  }

  def getOrdersFromFile(ordersFile: String): Seq[Order] = {
    getOrdersFromLines(Source.fromResource(ordersFile).getLines().toSeq)
  }

  def getClientBalancesFromLines(lines: Seq[String]): Map[(String, Currency), Money[Currency]] = {
    def balance(clientName: String, currency: Currency, volume: Double) = (clientName -> currency) -> Money(volume, currency)

    lines.foldLeft(Map.empty[(String, Currency), Money[Currency]]) { (balances, line) =>
      val Array(clientName, usd, a, b, c, d) = line.split("\t")
      val seq =
        (Seq(
          balance(clientName, USD, usd.toDouble),
          balance(clientName, A, a.toDouble),
          balance(clientName, B, b.toDouble),
          balance(clientName, C, c.toDouble),
          balance(clientName, D, d.toDouble)))

      balances ++ seq
    }
  }

  def getOrdersFromLines(lines: Seq[String]): Seq[Order] = {
    lines.zipWithIndex.map { case (line, index) =>
      val Array(clientName, orderType, currencyName, price, volume) = line.split("\t")

      Order(
        index,
        OrderType(orderType),
        clientName = clientName,
        Money(volume.toDouble, Currency(currencyName)),
        Money(price.toDouble, USD))
    }

  }
}

