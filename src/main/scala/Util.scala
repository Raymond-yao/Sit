import java.util.Properties

import scala.jdk.CollectionConverters._
import Model.{Commit, Diff}
import net.liftweb.json.JsonAST.{JField, JObject, JString, JValue}

object Util {

  def buildJsonList(c: Commit): List[JObject] = {
    List(c.toJson) ++ c.parent.map(buildJsonList).getOrElse(List())
  }

  /**
   * Given a base project and a list of commits, rebuild the current project
   *
   * @param baseProj the initial file of the project, as a Properties object
   * @param headCommit the newest commit, aka HEAD pointer
   *
   * @return a new Properties object with all commits difference applied
   * */
  def rebuild(baseProj: Properties, headCommit: Commit): Properties = headCommit.foldLeft(
    (someProp, someCommit) => {
      val propAfterChange = clone(someProp)
      val diffs = someCommit.diff
      diffs.deleted.keySet.foreach(propAfterChange.remove(_))
      diffs.added.foreach {
        case (k, v) => propAfterChange.setProperty(k, v)
      }

      propAfterChange
    }, baseProj)

  def findFieldToVal[T](fields: List[JField], key: String): T = {
    fields.find(_.name == key).get.value.asInstanceOf[T]
  }

  def rebuildCommit(fields: List[JField], parentCommit: Option[Commit]): Option[Commit] = {
    val diffJson = findFieldToVal[JObject](fields, "diff")
    val diff = Diff.fromJson(diffJson)
    val id = findFieldToVal[JString](fields, "id").s
    val commitMessage = findFieldToVal[JString](fields, "message").s
    val timestamp = findFieldToVal[JString](fields, "timestamp").s.toLong

    Some(Commit(id, parentCommit, diff, commitMessage, timestamp))
  }

  def rebuildCommitList(lst: List[JValue]): Option[Commit] = {
    lst.foldRight[Option[Commit]](None) {
      case (jVal, commitOpt) =>
        jVal match {
          case JObject(lst: List[JField]) => rebuildCommit(lst, commitOpt)
        }
    }
  }

  /**
   * Return a deep copy of the given properties object, I do this to strict the immutability of the structure
   **/
  def clone(prop: Properties): Properties = {
    val newProp = new Properties()
    prop.stringPropertyNames().forEach(name => newProp.setProperty(name, prop.getProperty(name)))
    newProp
  }

  /**
   * compute the difference of two Properties object,
   * E.g.
   * aProp: (
   *    "a" -> "value 1"
   *    "b" -> "value 2"
   *    "c" -> "value 3"
   * )
   *
   * bProp: (
   *    "b" -> "value 2"
   *    "c" -> "value 777"
   * )
   *
   * findKeyDiff(aProp, bProp) will be
   * Map(
   *  "a" -> "value 1"
   *  "c" -> "value 3"
   *  )
   *
   *  findKeyDiff(bProp, aProp) will be
   *  Map(
   *    "c" -> "value 777"
   *  )
   * */
  private def findKeyDiff(aProp: Properties, bProp: Properties): Map[String, String] = {
    aProp
      .stringPropertyNames()
      .asScala
      .filter(name => !bProp.containsKey(name) || !bProp.getProperty(name).equals(aProp.getProperty(name)))
      .map(name => name -> aProp.getProperty(name))
      .view.toMap
  }

  def compare(base: Properties, newProp: Properties): Diff = {
    Diff(
      findKeyDiff(base, newProp),
      findKeyDiff(newProp, base)
    )
  }
}
