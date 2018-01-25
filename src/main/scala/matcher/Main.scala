package matcher

object Main {
  val `clients.txt` = "clients.txt"
  val `orders.txt` = "orders.txt"
  val `result.txt` = "result.txt"

  def main(args: Array[String]): Unit = {

    val clientBalances = Parsing.getClientBalancesFromFile(`clients.txt`)
    val orders = Parsing.getOrdersFromFile(`orders.txt`)

    val matcher = new Matcher
    val (newBalances, _) = matcher.`match`(Balances(clientBalances), orders)

    newBalances.toRows.foreach(println)
  }
}