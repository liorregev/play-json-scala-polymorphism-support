package com.github.liorregev.pushbullet

import play.api.libs.json._

import scala.reflect.ClassTag

package object serialization {
  implicit def pairToSerializableADTClass[Parent, T <: Parent : ClassTag](pair: (String, OFormat[T])):
  SerializableADTClass[Parent, T] = {

    val (typeName, format) = pair
    SerializableADTClass[Parent, T](typeName, format)
  }

  implicit def pairToSerializableADTObject[T](pair: (String, T)): SerializableADTObject[T] = {
    val (typeName, instance) = pair
    SerializableADTObject[T](typeName, instance)
  }

  def formatFor[T](formatters: SerializableADT[T]*): OFormat[T] = formatForWithName("type", formatters: _*)

  def formatForWithName[T](fieldName: String, formatters: SerializableADT[T]*): OFormat[T] = new OFormat[T] {
    private val read =
      formatters.map(_.reads(fieldName)).fold(PartialFunction.empty)(_ orElse _).lift

    private val write =
      formatters.map(_.writes(fieldName)).fold(PartialFunction.empty)(_ orElse _)

    override def reads(json: JsValue): JsResult[T] =
      read(json).getOrElse {
        JsError(s"Unable to determine which type to use for $fieldName '${json \ fieldName toOption}'. Did you forget to manually add it to the list of serializers?")
      }

    override def writes(o: T): JsObject =
      write(o)
  }

  def snakeCaseFormat[T](originalFormat: OFormat[T]): OFormat[T] = new OFormat[T] {
    override def writes(o: T): JsObject =
      JsObject(originalFormat.writes(o)
        .as[Map[String, JsValue]]
        .map {
          case (key, value) => (camelToUnderscores(key), value)
        })

    override def reads(json: JsValue): JsResult[T] = json match {
      case data: JsObject =>
        originalFormat.reads(JsObject(data.as[Map[String, JsValue]].map {
          case (key, value) => (underscoreToCamel(key), value)
        }))
      case v => originalFormat.reads(v)
    }
  }

  def camelToUnderscores(name: String): String = "[A-Z\\d]".r.replaceAllIn(name, {m =>
    "_" + m.group(0).toLowerCase()
  })

  def underscoreToCamel(name: String): String = "_([a-z\\d])".r.replaceAllIn(name, {m =>
    m.group(1).toUpperCase()
  })

  def flattenFieldToObject(field: String, prefix: String = "")(obj: JsObject): JsObject = {
    (obj \ field).asOpt[JsObject]
      .map(_.fields.map(f => prefix + f._1 -> f._2))
      .map(fields => JsObject(fields) ++ obj - field)
      .getOrElse(obj)
  }
}