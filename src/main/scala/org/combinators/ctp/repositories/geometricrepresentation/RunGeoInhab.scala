package org.combinators.ctp.repositories.geometricrepresentation

import com.typesafe.scalalogging.LazyLogging
import io.circe.parser.decode
import org.combinators.cls.interpreter._
import org.combinators.cls.types.Type
import org.combinators.cls.types.syntax._
import org.combinators.ctp.repositories.protocol.UnityMeshData
import org.eclipse.paho.client.mqttv3.{IMqttMessageListener, MqttClient, MqttMessage}

class RunGeoInhab(geo: UnityMeshData, client: MqttClient) extends LazyLogging{
 /* lazy val repo = new GeometricDataFromUnity(geo)
  lazy val Gamma = repo.reflected
  logger.debug(Gamma.combinators.map { case (c, ty) => s"$c : $ty" }.mkString("{ ", ",\n", "}"))
  logger.debug(s"|- ? : ${ReflectedRepository.nativeTypeOf[repo.nativeTarget]} :&: ${repo.semanticTarget}")
  lazy val results: InhabitationResult[repo.nativeTarget] = Gamma.inhabit[repo.nativeTarget](repo.semanticTarget)

  logger.debug("Grammar:")
  logger.debug(
    results
      .grammar
      .map { case (ty, options) => s"$ty ::= ${options.mkString("", " | ", ";")}" }
      .mkString("{", "\n", "}"))

  lazy val resultIterator: Iterator[Seq[Movement]] = new Iterator[Seq[Movement]] {
    var pos: Int = 0
    override def hasNext: Boolean = true
    override def next: Seq[Movement] = pos.synchronized {
      val result = results.interpretedTerms.index(pos)
      pos += 1
      if (results.size.exists(s => pos >= s)) {
        pos = 0
      }
      result
    }
  }

  val onMessage: IMqttMessageListener = new IMqttMessageListener {
    override def messageArrived(topic: String, message: MqttMessage): Unit = {
      decode[GetSolutions](new String(message.getPayload)).foreach {
        case GetSolutions(maxCount) =>
          logger.debug(s"Received request: ${new String(message.getPayload)}")
          (0 until maxCount).foreach { _ =>
            client.synchronized {
              if (resultIterator.hasNext) {
                val next = Solution(resultIterator.next()).asJson.toString
                logger.debug(s"Sending: $next")
                client.publish(task.topicForSolutions, next.getBytes, 2, true)
              }
            }
          }
      }
    }
  }

  client.subscribe(task.topicForRequests, 2, onMessage)*/
}