package com.github.liorregev.pushbullet.serialization

import play.api.libs.json._

import scala.reflect.ClassTag

final case class SerializableADTClass[Parent, T <: Parent : ClassTag](typeName: String, format: OFormat[T])
  extends SerializableADT[Parent] {

  def reads(fieldName: String): PartialFunction[JsValue, JsResult[Parent]] = {
    case js: JsObject if (js \ fieldName).as[String] == typeName =>
      format.reads(js)
  }

  def writes(fieldName: String): PartialFunction[Parent, JsObject] = {
    case obj: T =>
      format.writes(obj) + (fieldName -> JsString(typeName))
  }
}