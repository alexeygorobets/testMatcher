package matcher


case class Balances(private val underlying: Map[(String, Currency), Money[Currency]]){

  def update(order: Order): Balances = {
    def exec(order: Order) = {
      val baseAsset = order.price * order.asset.value
      order.orderType match {
        case Sell =>
          (- order.asset, baseAsset)
        case Buy =>
          (order.asset, - baseAsset)
      }
    }

    val clientName = order.clientName
    val key     = clientName -> order.currency
    val usdKey  = clientName -> USD

    val asset = underlying(key)
    val usd = underlying(usdKey)
    val (assetChange, usdChange) = exec(order)

    Balances(underlying
      .updated(key, asset + assetChange)
      .updated(usdKey, usd + usdChange))
  }

  def toRows: Seq[String] = {

    underlying.keySet.toSeq.map { case (clientName, _) =>
      val usd = underlying(clientName -> USD).value
      val a = underlying(clientName -> A).value
      val b = underlying(clientName -> B).value
      val c = underlying(clientName -> C).value
      val d = underlying(clientName -> D).value

      s"$clientName\t$usd\t$a\t$b\t$c\t$d"

    }.distinct.sorted

  }

}
