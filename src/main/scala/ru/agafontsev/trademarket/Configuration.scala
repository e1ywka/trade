package ru.agafontsev.trademarket

import com.typesafe.config.Config

trait Configuration {
  import scala.collection.convert.ImplicitConversionsToScala._

  def config: Config

  def getProducts: Seq[String] = config.getStringList("app.products")

  def getClientFilePath: String = config.getString("app.clients.file")

  def getOrdersFilePath: String = config.getString("app.orders.file")

  def getResultFilePath: String = "result.txt"
}
