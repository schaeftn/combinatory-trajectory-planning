package org.combinators.ctp.repositories.taxkinding

import org.combinators.cls.types.{Constructor, Type, Variable}

trait Protocol {
  val p_unityConnectionProperties_type: Type = Constructor("p_unityConnectionProperties_type") //Connection properties for Unity
  val p_mqttClient_type: Type = Constructor("p_mqttClient_type") // MqttClient
  val p_unitySceneConnectionValues_three_d_type: Type = Constructor("p_unitySceneConnectionValues_three_d_type") // Topic values for Unity Connection
  val p_unitySceneConnectionValues_two_d_type: Type = Constructor("p_unitySceneConnectionValues_two_d_type") // Topic values for Unity Connection
  val p_unitySceneAgent_type: Type = Constructor("p_unitySceneAgent_type") // Read Scene from unity
  val p_unityResult_type: Type = Constructor("p_unityResult_type") // Display result in Unity
}
