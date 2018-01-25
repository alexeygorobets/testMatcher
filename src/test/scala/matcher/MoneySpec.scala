package matcher

import org.specs2.Specification

class MoneySpec extends Specification { def is = s2"""
   Money ops correct $moneyOpsSpec"""

  def moneyOpsSpec = {
    val `5 usd` = Money(5, USD)
    val `7 usd` = Money(7, USD)

    `5 usd` must_!= Money(5, A)
    -`5 usd` must_== Money(-5, USD)

    `5 usd` * 3 must_== Money(15, USD)

    `7 usd` + `5 usd` must_== Money(15, USD)
    `7 usd` - `5 usd` must_== Money(2, USD)
    `7 usd` - `5 usd` must_== Money(2, USD)
    `7 usd` -`5 usd` must_== Money(2, USD)
    `7 usd` + (-`5 usd`) must_!== `7 usd` -`5 usd`

    `5 usd` <= `7 usd` must_== true
    `7 usd` >= `5 usd` must_== true
  }
}