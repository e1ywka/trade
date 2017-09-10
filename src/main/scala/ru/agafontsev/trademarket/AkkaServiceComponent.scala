package ru.agafontsev.trademarket

import akka.actor.ActorSystem

trait AkkaServiceComponent {
  def actorSystem: ActorSystem
}
