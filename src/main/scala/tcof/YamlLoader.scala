package tcof

import scala.collection.JavaConverters._
import java.nio.file.{Files, Paths}
import java.time.{LocalDateTime, ZoneId}
import java.util

import org.yaml.snakeyaml.Yaml
import tcof.traits.map2d.Position

object YamlLoader {
  type YamlDict = Map[String, YamlValue]

  class YamlValue(val value: AnyRef) {
    def asMap[T](implicit upcastFn: YamlValue => T): Map[String, T] = YamlValue.toYamlDict(this).mapValues(upcastFn)
    def asYamlDict: YamlDict = YamlValue.toYamlDict(this)
    def asList[T](implicit upcastFn: YamlValue => T): List[T] = YamlValue.toList(this)(upcastFn)
  }

  object YamlValue {
    def apply(value: AnyRef) = new YamlValue(value)

    implicit def toYamlDict(value: YamlValue): YamlDict = value.value.asInstanceOf[util.Map[String, AnyRef]].asScala.mapValues(YamlValue(_)).toMap
    implicit def toList[T](value: YamlValue)(implicit upcastFn: YamlValue => T): List[T] = value.value.asInstanceOf[util.List[AnyRef]].asScala.map(YamlValue(_)).toList.map(upcastFn)
    implicit def toSet[T](value: YamlValue)(implicit upcastFn: YamlValue => T): Set[T] = toList(value)(upcastFn).toSet
    implicit def toString(value: YamlValue): String = value.value.toString
    implicit def toInt(value: YamlValue): Int = value.value.asInstanceOf[Int]
    implicit def toBoolean(value: YamlValue): Boolean = value.value.asInstanceOf[Boolean]
    implicit def toLocalDateTime(value: YamlValue): LocalDateTime = LocalDateTime.ofInstant(value.value.asInstanceOf[util.Date].toInstant, ZoneId.of("UTC"))
    implicit def toPosition(value: YamlValue): Position = {
      val List(x, y) = value.asList[Int]
      Position(x, y)
    }
  }

  val yaml = new Yaml()
  def loadYaml(path: String) = YamlValue(yaml.load(Files.newBufferedReader(Paths.get(path))))
}
