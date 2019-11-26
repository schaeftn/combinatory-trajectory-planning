package org.combinators.ctp.repositories.toplevel

import java.util.Properties
import org.combinators.ctp.repositories.scene.{CtpSceneConnectionValues, CtpSceneListener}
import org.eclipse.paho.client.mqttv3.{MqttClient, MqttConnectOptions}

object ClsMqttSceneAgent extends App  {
  val connectionSettings = new Properties()
  connectionSettings.load(getClass.getClassLoader.getResourceAsStream("mqtt.properties"))

  val broker = connectionSettings.getProperty("org.combinators.ctp.broker")
  val clientId = connectionSettings.getProperty("org.combinators.ctp.clientId")
  val ctpSceneRequest = connectionSettings.getProperty("org.combinators.ctp.ctpSceneRequest")
  val ctpSceneResponse = connectionSettings.getProperty("org.combinators.ctp.ctpSceneResponse")

  println(s"""Connecting to broker "$broker" as "$clientId"""")
  val client: MqttClient = new MqttClient(broker, clientId)
  val options: MqttConnectOptions = new MqttConnectOptions()
  options.setAutomaticReconnect(true)
  options.setCleanSession(true)
  options.setConnectionTimeout(10)
  options.setMaxInflight(1000)
  client.connect(options)

  println(s"Receiving tasks from topic: $ctpSceneRequest")

  CtpSceneListener(client, CtpSceneConnectionValues(ctpSceneRequest, ctpSceneResponse)).subscribe

  println("Press <Enter> to exit.")
  scala.io.StdIn.readLine()

  println(s"Disconnecting $clientId from $broker")
  client.disconnect()
  client.close()
}