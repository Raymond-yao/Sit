package Model

import java.util.UUID.randomUUID

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
}
