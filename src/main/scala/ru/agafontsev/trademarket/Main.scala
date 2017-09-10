package ru.agafontsev.trademarket

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Main extends App
  with Configuration
  with AllStuff
  with MatcherComponent
  with FileParserComponent
  with ExecutionContextComponent {

  override val config = ConfigFactory.load()

  val as = ActorSystem("waves-test", config)

  override implicit def ec: ExecutionContext = as.dispatcher

  trait AkkaServiceComponentImpl extends AkkaServiceComponent {
    override def actorSystem: ActorSystem = as
  }

  trait AkkaExecutionContextComponent extends ExecutionContextComponent {
    override implicit def ec: ExecutionContext = as.dispatcher
  }

  override val matcher = new MatcherImpl(getProducts)
    with AkkaServiceComponentImpl
    with AkkaExecutionContextComponent

  override val fileParser = new FileParserImpl with AkkaExecutionContextComponent with Configuration {
    override def config = Main.config
  }

  run().onComplete {
    case Success(_) => System.exit(0)
    case Failure(e) => println(e); System.exit(1)
  }
}

trait AllStuff  {
  this: MatcherComponent
    with FileParserComponent
    with ExecutionContextComponent =>

  def run(): Future[Unit] = {
    for {
      accs <- fileParser.readClientsFile()
      orders <- fileParser.readOrdersFile()
      _ <- Future.sequence(orders.map(matcher.placeOrder))
      ledger <- matcher.getLedger()
      newAccs = Account.compute(accs, ledger.tx)
      _ <- fileParser.writeResultFile(newAccs.sortBy(_.client))
    } yield ()
  }
}