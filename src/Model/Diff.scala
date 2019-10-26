package Model

case class Diff(added: Map[String, String], deleted: Map[String, String]) {

  def revert: Diff = {
    this.copy(added = this.deleted, deleted = this.added)
  }
}
