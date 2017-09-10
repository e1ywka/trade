package ru.agafontsev.trademarket

import java.nio.file.{Files, Paths}

import ru.agafontsev.trademarket.Matcher.{Order, OrderTypes}

import scala.concurrent.Future


trait FileParser {

  def readClientsFile(): Future[Seq[Account]]
  def readOrdersFile(): Future[Seq[Order]]
  def writeResultFile(accounts: Seq[Account]): Future[Unit]
}

trait FileParserComponent {
  def fileParser: FileParser
}

object FileParser {

  def convertToAccount(str: String, products: Seq[String]): Account = {
    val elems = str.split('\t')
    val accountProducts = Map(
      products(0) -> elems(2).toInt,
      products(1) -> elems(3).toInt,
      products(2) -> elems(4).toInt,
      products(3) -> elems(5).toInt,
    )
    Account(elems(0), accountProducts, elems(1).toInt)
  }

  def convertToOrder(str: String): Order = {
    val elems = str.split('\t')
    Order(elems(0), OrderTypes.withName(elems(1)), elems(2), elems(3).toInt, elems(4).toInt)
  }

  def convertToStr(account: Account, products: Seq[String]): String = {
    val prodStr = products.map(account.products(_)).mkString("\t")
    s"${account.client}\t${account.balance}\t$prodStr"
  }
}

class FileParserImpl extends FileParser {
  this: Configuration
    with ExecutionContextComponent =>

  import scala.collection.JavaConverters._
  import FileParser._

  override def readClientsFile(): Future[Seq[Account]] = Future {
    Files.readAllLines(Paths.get(getClientFilePath)).asScala
      .map(convertToAccount(_, getProducts))
  }

  override def readOrdersFile(): Future[Seq[Order]] = Future {
    Files.readAllLines(Paths.get(getOrdersFilePath)).asScala
        .map(convertToOrder)
  }

  override def writeResultFile(accounts: Seq[Account]) = Future {
    val result = accounts.map(convertToStr(_ , getProducts))
    Files.write(Paths.get(getResultFilePath), result.asJava)
  }
}