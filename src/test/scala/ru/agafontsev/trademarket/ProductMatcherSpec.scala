package ru.agafontsev.trademarket

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike}
import ru.agafontsev.trademarket.Matcher._
import ru.agafontsev.trademarket.ProductMatcher.Command

class ProductMatcherSpec(_system: ActorSystem) extends TestKit(_system)
  with ImplicitSender
  with FlatSpecLike
  with BeforeAndAfterAll {

  def this() = this(ActorSystem("ProductMatcherSpec"))

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "Match orders" should "match" in {
    val matcher = system.actorOf(ProductMatcher.props("A"))

    matcher ! Command.SetAsk(Order("C1", OrderTypes.Buy, "A", 1, 1))
    expectMsg(OrderId(1))

    matcher ! Command.SetBid(Order("C2", OrderTypes.Sell, "A", 2, 1))
    expectMsg(OrderId(2))

    matcher ! Command.SetBid(Order("C3", OrderTypes.Sell, "A", 1, 1))
    expectMsg(OrderId(3))

    matcher ! Command.GetTransactions
    val ledger = expectMsgClass(classOf[Ledger])
    ledger.tx should contain(Transaction("A", "C1", 1, 1))
    ledger.tx should contain(Transaction("A", "C3", 1, -1))
    ledger.tx should have size(2)

    matcher ! Command.SetAsk(Order("C1", OrderTypes.Buy, "A", 3, 1))
    expectMsg(OrderId(4))

    matcher ! Command.GetTransactions
    val ledger2 = expectMsgClass(classOf[Ledger])
    ledger2.tx should contain(Transaction("A", "C1", 2, 1))
    ledger2.tx should contain(Transaction("A", "C2", 2, -1))
    ledger2.tx should have size(2)
  }

  it should "respect orders' order" in {
    val matcher = system.actorOf(ProductMatcher.props("A"))

    matcher ! Command.SetAsk(Order("C1", OrderTypes.Buy, "A", 1, 1))
    expectMsg(OrderId(1))
    matcher ! Command.SetAsk(Order("C2", OrderTypes.Buy, "A", 1, 1))
    expectMsg(OrderId(2))
    matcher ! Command.SetBid(Order("C3", OrderTypes.Sell, "A", 1, 1))
    expectMsg(OrderId(3))

    matcher ! Command.GetTransactions
    val ledger2 = expectMsgClass(classOf[Ledger])
    ledger2.tx should contain(Transaction("A", "C1", 1, 1))
    ledger2.tx should contain(Transaction("A", "C3", 1, -1))
    ledger2.tx should have size(2)
  }
}
