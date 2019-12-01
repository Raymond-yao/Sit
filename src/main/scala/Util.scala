import java.util.Properties
import scala.jdk.CollectionConverters._

import Model.{Commit, Diff}

object Util {

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
      diffs.added.foreach {
        case (k, v) => propAfterChange.setProperty(k, v)
      }

      diffs.deleted.keySet.foreach(propAfterChange.remove(_))
      propAfterChange
    }, baseProj)

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
