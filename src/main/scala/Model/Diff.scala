package Model

import net.liftweb.json.JsonAST.{JArray, JField, JObject, JString}

case class Diff(added: Map[String, String], deleted: Map[String, String]) {

  def revert: Diff = {
    this.copy(added = this.deleted, deleted = this.added)
  }

  def convertMapToJson(map: Map[String, String]): JArray = {
    JArray(map.toList.map {
      case (k, v) =>
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

object Diff {

  private def jsonToMap(jArr: JArray): Map[String, String] = {
    val listOfEntries = jArr.arr.map(_.asInstanceOf[JObject])
    listOfEntries.map {
      entryObj =>
        val key = entryObj.findField(_.name == "key").get.value.asInstanceOf[JString].s
        val value = entryObj.findField(_.name == "value").get.value.asInstanceOf[JString].s

        key -> value
    }.toMap
  }

  def fromJson(jobj: JObject): Diff = {
    val fields = jobj.obj
    val addedJson = fields.find(_.name == "added").get.value.asInstanceOf[JArray]
    val deletedJson = fields.find(_.name == "deleted").get.value.asInstanceOf[JArray]

    Diff(jsonToMap(addedJson), jsonToMap(deletedJson))
  }
}
