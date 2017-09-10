package ru.agafontsev.trademarket

import org.scalatest.{FlatSpec, Matchers}
import ru.agafontsev.trademarket.Matcher.{Order, OrderTypes}

class FileParserSpec extends FlatSpec with Matchers {

  private val products = Seq("A", "B", "C", "D")

  "FileParser" should "parse clients record" in {
    val str = "C1\t1000\t130\t240\t760\t320"

    val expectedAccount = Account(client = "C1",
      products = Map(
        "A" -> 130,
        "B" -> 240,
        "C" -> 760,
        "D" -> 320
      ),
      balance = 1000
    )

    FileParser.convertToAccount(str, products) should be(expectedAccount)
  }

  it should "parse orders record" in {
    val str = "C8\tb\tC\t15\t4"
    val expectedOrder = Order(client = "C8",
      orderType = OrderTypes.Buy,
      productId = "C",
      price = 15,
      amount = 4
    )
    FileParser.convertToOrder(str) should be(expectedOrder)
  }

  it should "serialize account" in {
    val account = Account(client = "C1",
      products = Map(
        "A" -> 130,
        "B" -> 240,
        "C" -> 760,
        "D" -> 320
      ),
      balance = 1000
    )
    val expectedStr = "C1\t1000\t130\t240\t760\t320"
    FileParser.convertToStr(account, products) should be(expectedStr)
  }
}
