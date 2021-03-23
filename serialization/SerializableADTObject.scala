package com.github.liorregev.pushbullet.serialization

import play.api.libs.json._

final case class SerializableADTObject[T](typeName: String, instance: T) extends SerializableADT[T] {
  def reads(fieldName: String): PartialFunction[JsValue, JsResult[T]] = {
    case obj: JsObject if (obj \ fieldName).as[String] == typeName =>
      JsSuccess(instance)
  }

  def writes(fieldName: String): PartialFunction[T, JsObject] = {
    case obj if obj == instance =>
      JsObject(Map(fieldName -> JsString(typeName)))
  }
}
