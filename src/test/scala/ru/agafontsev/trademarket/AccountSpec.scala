package ru.agafontsev.trademarket

import org.scalatest.{FlatSpec, Matchers}

class AccountSpec extends FlatSpec with Matchers {

  "AccountBuffer" should "handle buying tx" in new Wiring {
    val accountBuffer = new AccountBuffer(account)
    accountBuffer.change("A", 10, 1)
    accountBuffer.balance should be(1000 - 10 * 1)
    accountBuffer.products("A") should be(130 + 10)
  }

  it should "handle selling tx" in new Wiring {
    val accountBuffer = new AccountBuffer(account)
    accountBuffer.change("A", -10, 1)
    accountBuffer.balance should be(1000 + 10 * 1)
    accountBuffer.products("A") should be(130 - 10)
  }

  private trait Wiring {
    val account = Account(client = "C1",
      products = Map(
        "A" -> 130,
        "B" -> 240,
        "C" -> 760,
        "D" -> 320
      ),
      balance = 1000
    )
  }
}
