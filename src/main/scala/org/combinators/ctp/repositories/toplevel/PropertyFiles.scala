package org.combinators.ctp.repositories.toplevel

import java.util.Properties

import io.circe.{Encoder, Json}
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

trait PropertyFiles {
  val mqttProperties = new Properties()
  mqttProperties.load(getClass.getClassLoader.getResourceAsStream("mqtt.properties"))

  val pythonInteropProperties = new Properties()
  pythonInteropProperties.load(getClass.getClassLoader.getResourceAsStream("pythoninterop.properties"))

  val problemsProperties = new Properties()
  problemsProperties.load(getClass.getClassLoader.getResourceAsStream("problems.properties"))
}

object PropertyFiles extends PropertyFiles{
}


