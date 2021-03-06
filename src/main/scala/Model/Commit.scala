package Model

import java.util.Properties
import java.util.UUID.randomUUID

import net.liftweb.json.JsonAST.{JObject,JField, JNull, JString}


case class Commit(
                   id: String,
                   parent: Option[Commit],
                   diff: Diff,
                   commitMessage: String,
                   timestamp: Long
                 ) {

  private def generateCommitID: String = {
    randomUUID().toString
  }

  def addNext(diff: Diff, commitMessage: String): Commit = {
    Commit(generateCommitID, Some(this), diff, commitMessage, System.currentTimeMillis())
  }

  /**
   * Give Commit the ability to "foldl": given a base Properties, we calculate it with the first commit, the second
   * commit, the third commit ... up to the HEAD commit.
   * The implementation is a little bit awkward as it is hard (nearly impossible) to do a doubly linkedlist using
   * immutable case class
   *
   * @param helper a function (Properties, Commit) -> Properties, as the lambda in  "foldl" in racket
   * @param base the Base Properties object
   *
   * @return the new computed Properties object with all commits considered.
   * */
  def foldLeft(helper: (Properties, Commit) => Properties, base: Properties): Properties = {
    parent.map(c => helper(c.foldLeft(helper, base), this)).getOrElse(helper(base, this))
  }

  def toJson: JObject = {
    JObject(List(
      JField("id", JString(id)),
      JField("diff", diff.toJson),
      JField("message", JString(commitMessage)),
      JField("timestamp", JString(timestamp.toString)),
      JField("parent_id", parent.map(p => JString(p.id)).getOrElse(JNull))
    ))
  }
}

object Commit {

  def head(diff: Diff, commitMessage: String): Commit = {
    Commit(randomUUID().toString, None, diff, commitMessage, System.currentTimeMillis())
  }
}
