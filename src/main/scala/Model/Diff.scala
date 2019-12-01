package Model

import net.liftweb.json.JsonAST.{JArray, JField, JObject, JString}

case class Diff(added: Map[String, String], deleted: Map[String, String]) {

  def revert: Diff = {
    this.copy(added = this.deleted, deleted = this.added)
  }

  def convertMapToJson(map: Map[String, String]): JArray = {
    JArray(map.toList.map {
      case (k,v) =>
        JObject(List(
          JField("key", JString(k)),
          JField("value", JString(v))
        ))
    })
  }

  def toJson: JObject = {
    JObject(List(
      JField("added", convertMapToJson(added)),
      JField("deleted", convertMapToJson(deleted))
    ))
  }
}
