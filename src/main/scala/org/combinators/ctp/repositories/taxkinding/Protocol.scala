package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Type, Variable}

trait Protocol {
  val p_unityConnectionProperties_type: Type = Constructor("p_unityConnectionProperties_type") //Connection properties for Unity
  val p_mqttCirceClient_type: Type = Constructor("p_mqttCirceClient_type") // MqttClient
  val p_mqttAkkaConnxSettings_type: Type = Constructor("p_mqttAkkaConnxSettings_type") // MqttClient
  val p_mqttAkkaSource_type: Type = Constructor("p_mqttAkkaSource_type") // MqttClient
  val p_mqttAkkaSink_type: Type = Constructor("p_mqttAkkaSink_type") // MqttClient
  val p_unitySceneConnectionValues_three_d_type: Type = Constructor("p_unitySceneConnectionValues_three_d_type") // Topic values for Unity Connection
  val p_unitySceneConnectionValues_two_d_type: Type = Constructor("p_unitySceneConnectionValues_two_d_type") // Topic values for Unity Connection
  val p_mqttAkkaComposition_type: Type = Constructor("p_mqttAkkaComposition_type") // reads/writes data via mqtt
  val p_fileToAkka_type: Type = Constructor("p_fileToAkka_type") // files in, mqtt data out
}
