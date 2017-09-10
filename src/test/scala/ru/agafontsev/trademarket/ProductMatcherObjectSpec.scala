package ru.agafontsev.trademarket

import org.scalatest.FlatSpec
import ru.agafontsev.trademarket.Matcher.{Order, OrderTypes}
import ru.agafontsev.trademarket.ProductMatcher.Bid

import scala.collection.mutable

class ProductMatcherObjectSpec extends FlatSpec {

  import org.scalatest.Matchers._

  "Adding bid to empty map" should "add new entry" in {
    val bids = mutable.TreeMap.empty[Int, Seq[Bid]]
    val price = 1
    val bid = Bid(Order("client", OrderTypes.Sell, "A", price, 1), 1)

    ProductMatcher.addBid(bid, bids)
    bids.get(price) should matchPattern {
      case Some(Seq(`bid`)) =>
    }

    ProductMatcher.removeBid(bid, bids)
    bids.get(price) should be(None)
  }

  "Adding two bids with same price" should "add single entry" in {
    val bids = mutable.TreeMap.empty[Int, Seq[Bid]]
    val price = 1
    val bid1 = Bid(Order("client", OrderTypes.Sell, "A", price, 1), 1)
    val bid2 = Bid(Order("client", OrderTypes.Sell, "A", price, 1), 2)

    ProductMatcher.addBid(bid1, bids)
    ProductMatcher.addBid(bid2, bids)
    bids.get(price) should matchPattern {
      case Some(Seq(`bid1`, `bid2`)) =>
    }

    ProductMatcher.removeBid(bid1, bids)
    bids.get(price) should matchPattern {
      case Some(Seq(`bid2`)) =>
    }
  }

  "Adding two bids with defferent prices" should "add two entries" in {
    val bids = mutable.TreeMap.empty[Int, Seq[Bid]]
    val priceSmaller = 1
    val priceHigher = 2
    val bidWithSmallerPrice = Bid(Order("client", OrderTypes.Sell, "A", priceSmaller, 1), 1)
    val bidWithHighPrice = Bid(Order("client", OrderTypes.Sell, "A", priceHigher, 1), 2)

    ProductMatcher.addBid(bidWithSmallerPrice, bids)
    ProductMatcher.addBid(bidWithHighPrice, bids)
    bids.get(priceSmaller) should matchPattern {
      case Some(Seq(`bidWithSmallerPrice`)) =>
    }
    bids.get(priceHigher) should matchPattern {
      case Some(Seq(`bidWithSmallerPrice`, `bidWithHighPrice`)) =>
    }
  }

  "Ordering of placing bids" should "matter" in {
    val bids = mutable.TreeMap.empty[Int, Seq[Bid]]
    val priceSmall = 1
    val priceHigh = 2
    val bidWithSmallPrice = Bid(Order("client", OrderTypes.Buy, "A", priceSmall, 1), 1)
    val bidWithHighPrice = Bid(Order("client", OrderTypes.Buy, "A", priceHigh, 1), 2)

    ProductMatcher.addBid(bidWithHighPrice, bids)
    ProductMatcher.addBid(bidWithSmallPrice, bids)
    bids.get(priceSmall) should matchPattern {
      case Some(Seq(`bidWithSmallPrice`)) =>
    }
    bids.get(priceHigh) should matchPattern {
      case Some(Seq(`bidWithHighPrice`, `bidWithSmallPrice`)) =>
    }
  }
}
