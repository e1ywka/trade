package ru.agafontsev.trademarket

import ru.agafontsev.trademarket.Account.AccountChange
import ru.agafontsev.trademarket.Matcher.Transaction

import scala.collection.mutable

object Account {
  type AccountChange = (String, Int, Int) // product, amount, price

  def compute(accounts: Seq[Account], tx: Seq[Transaction]): Seq[Account] = {
    val buffers = accounts.map(a => a.client -> new AccountBuffer(a)).toMap
    tx.foldLeft(buffers) { (buf, t) =>
      val b = buf(t.client)
      b.change(t.productId, t.amount, t.price)
      buf
    }.map { accountBuffer =>
      val (client, buffer) = accountBuffer
      Account(client, buffer.products.toMap, buffer.balance)
    }.toSeq
  }
}

case class Account(client: String, products: Map[String, Int], balance: Int)

private[trademarket] class AccountBuffer(var products: mutable.Map[String, Int], var balance: Int) {

  def this(a: Account) = this(mutable.Map(a.products.toSeq: _*), a.balance)

  def change(change: AccountChange): Unit = {
    val (productId, amount, price) = change
    val curAmount = products.getOrElse(productId, 0)
    products.update(productId, curAmount + amount)
    balance += -amount * price
  }
}