package org.combinators.ctp.repositories.toplevel

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}

trait AkkaImplicits extends EncodeImplicits{
  implicit val as: ActorSystem = ActorSystem()
  implicit val am: Materializer = ActorMaterializer()
}


