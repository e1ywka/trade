package ru.agafontsev.trademarket

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import ru.agafontsev.trademarket.Matcher.OrderTypes.OrderType
import ru.agafontsev.trademarket.Matcher._
import ru.agafontsev.trademarket.ProductMatcher.Command

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}

import scala.concurrent.duration._

trait Matcher {
  def placeOrder(order: Order): Future[OrderId]
  def getLedger(): Future[Ledger]
}

object Matcher {
  case class Order(client: String, orderType: OrderType, productId: String, price: Int, amount: Int) {
    require(price > 0)
    require(amount > 0)
  }

  object OrderTypes extends Enumeration {
    type OrderType = Value

    val Buy = Value("b")
    val Sell = Value("s")
  }

  case class OrderId(id: Int)

  case class Ledger(tx: Seq[Transaction])

  case class Transaction(productId: String, client: String, price: Int, amount: Int)

}

trait MatcherComponent {
  def matcher: Matcher
}

class MatcherImpl(products: Seq[String]) extends Matcher {
  this: AkkaServiceComponent
    with ExecutionContextComponent =>

  private implicit val timeout = Timeout(3.seconds)

  private lazy val productMatchers: Map[String, ActorRef] = products.map { productId =>
    productId -> actorSystem.actorOf(ProductMatcher.props(productId))
  }.toMap

  override def placeOrder(order: Order): Future[OrderId] = {
    getProductMatcher(order.productId).flatMap { m =>
      val result = order.orderType match {
        case OrderTypes.Sell => m ? Command.SetBid(order)
        case OrderTypes.Buy => m ? Command.SetAsk(order)
      }
      result.mapTo[OrderId]
    }
  }

  override def getLedger(): Future[Ledger] =
    Future.sequence(productMatchers.values.map(m => (m ? Command.GetTransactions).mapTo[Ledger]))
      .map(ls => Ledger(ls.flatMap(_.tx).toSeq))

  private def getProductMatcher(productId: String) = Future.fromTry(productMatchers.get(productId) match {
    case Some(matcher) => Success(matcher)
    case None => Failure(new Exception(s"unknown product $productId"))
  })
}

private[trademarket] object ProductMatcher {
  object Command {
    case class SetBid(order: Order)
    case class SetAsk(order: Order)
    case object GetTransactions
  }

  case class Bid(order: Order, requestId: Int)

  case class Ask(order: Order, requestId: Int)


  def addBid(bid: Bid, bids: mutable.TreeMap[Int, Seq[Bid]]): Unit = {
    bids.filterKeys(_ >= bid.order.price).foreach { entry =>
      val (price, b) = entry
      bids.update(price, b :+ bid)
    }
    bids.getOrElseUpdate(
      bid.order.price,
      bids.filterKeys(_ < bid.order.price)
        .headOption
        .map(_._2)
        .fold(Seq(bid))(_ :+ bid)
    )
  }

  def addAsk(ask: Ask, asks: mutable.TreeMap[Int, Seq[Ask]]): Unit = {
    asks.filterKeys(_ <= ask.order.price).foreach { entry =>
      val (price, a) = entry
      asks.update(price, a :+ ask)
    }
    asks.getOrElseUpdate(
      ask.order.price,
      asks.filterKeys(_ > ask.order.price)
        .lastOption
        .map(_._2)
        .fold(Seq(ask))(_ :+ ask)
    )
  }

  def removeBid(bid: Bid, bids: mutable.TreeMap[Int, Seq[Bid]]): Unit = {
    bids.filterKeys(_ >= bid.order.price).foreach { entry =>
      val (price, b) = entry
      bids.update(price, b.filterNot(_ == bid))
    }
    if (bids.get(bid.order.price).exists(_.isEmpty)) {
      bids.remove(bid.order.price)
    }
  }

  def removeAsk(ask: Ask, asks: mutable.TreeMap[Int, Seq[Ask]]): Unit = {
    asks.filterKeys(_ <= ask.order.price).foreach { entry =>
      val (price, a) = entry
      asks.update(price, a.filterNot(_ == ask))
    }
    if (asks.get(ask.order.price).exists(_.isEmpty)) {
      asks.remove(ask.order.price)
    }
  }

  def props(productId: String): Props = Props(new ProductMatcher(productId))
}

private[trademarket] class ProductMatcher(productId: String) extends Actor {
  import ProductMatcher._

  private var orderId = 0

  private val bids: mutable.TreeMap[Int, Seq[Bid]] = mutable.TreeMap.empty
  private val asks: mutable.TreeMap[Int, Seq[Ask]] = mutable.TreeMap.empty

  private var transactions: Seq[Transaction] = Seq.empty

  def receive: Receive = {
    case Command.SetBid(bidOrder) =>
      setOrderId()

      val fittingAsksOpt = asks.filterKeys(_ >= bidOrder.price).lastOption.map(_._2)

      fittingAsksOpt.fold(addBid(Bid(bidOrder, orderId), bids)) { fittingAsks =>
        fittingAsks.collectFirst {
            case ask @ Ask(request, _)
              if request.amount == bidOrder.amount => ask
          }
          .fold(addBid(Bid(bidOrder, orderId), bids)) { matchingAsk =>
            val t1 = Transaction(productId, bidOrder.client, matchingAsk.order.price, -bidOrder.amount)
            val t2 = Transaction(productId, matchingAsk.order.client, matchingAsk.order.price, bidOrder.amount)
            transactions = transactions ++ Seq(t1, t2)
            removeAsk(matchingAsk, asks)
          }
      }

    case Command.SetAsk(askOrder) =>
      setOrderId()

      val fittingBidsOpt = bids.filterKeys(_ <= askOrder.price).headOption.map(_._2)

      fittingBidsOpt.fold(addAsk(Ask(askOrder, orderId), asks)) { fittingBids =>
        fittingBids.collectFirst {
            case bid @ Bid(request, _)
              if request.amount == askOrder.amount => bid
          }
          .fold(addAsk(Ask(askOrder, orderId), asks)) { matchingBid =>
            val t1 = Transaction(productId, askOrder.client, matchingBid.order.price, askOrder.amount)
            val t2 = Transaction(productId, matchingBid.order.client, matchingBid.order.price, -askOrder.amount)
            transactions = transactions ++ Seq(t1, t2)
            removeBid(matchingBid, bids)
          }
      }

    case Command.GetTransactions =>
      sender() ! Ledger(transactions)
      transactions = Seq.empty
  }

  private def setOrderId(): Unit = {
    orderId += 1
    sender() ! OrderId(orderId)
  }


}