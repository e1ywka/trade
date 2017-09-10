package ru.agafontsev.trademarket

import scala.concurrent.ExecutionContext


trait ExecutionContextComponent {
  implicit def ec: ExecutionContext
}
