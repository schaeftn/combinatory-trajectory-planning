package org.combinators.ctp.repositories.toplevel

import java.util.Properties
import org.combinators.ctp.repositories.boundingvolumes._
import org.eclipse.paho.client.mqttv3.{MqttClient, MqttConnectOptions}

object ClsMqttBvAgent extends App  {
  val connectionSettings = new Properties()
  connectionSettings.load(getClass.getResourceAsStream("mqtt.properties"))

  val broker = connectionSettings.getProperty("org.combinators.ctp.broker")
  val clientId = connectionSettings.getProperty("org.combinators.ctp.clientId")
  val bsRequestTopic = connectionSettings.getProperty("org.combinators.ctp.bsRequestTopic")
  val bbRequestTopic = connectionSettings.getProperty("org.combinators.ctp.bbRequestTopic")
  val bsResponseTopic = connectionSettings.getProperty("org.combinators.ctp.bsResponseTopic")
  val bbResponseTopic = connectionSettings.getProperty("org.combinators.ctp.bbResponseTopic")

  println(s"""Connecting to broker "$broker" as "$clientId"""")
  val client: MqttClient = new MqttClient(broker, clientId)
  val options: MqttConnectOptions = new MqttConnectOptions()
  options.setAutomaticReconnect(true)
  options.setCleanSession(true)
  options.setConnectionTimeout(10)
  options.setMaxInflight(1000)
  client.connect(options)

  println(s"Receiving tasks from topic: $bsRequestTopic")
  println(s"Receiving tasks from topic: $bbRequestTopic")

  CtpBbListener(client).subscribe
  CtpBsListener(client).subscribe

  println("Press <Enter> to exit.")
  scala.io.StdIn.readLine()

  println(s"Disconnecting $clientId from $broker")
  client.disconnect()
  client.close()

  
}