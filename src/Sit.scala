import java.io.{File, PrintWriter}

import Model.{Commit, Diff}
import java.nio.file.{Files, Paths}

import scala.util.Try

class Sit(
           private var head: Option[Commit],
           private val sitConfigPath: String
         ) {

  Try(readFromDisk).foreach(c => this.head = Some(c))

  /**
   * persist the current Model.Commit list to the directory, ideally it should be located in ./.sit
   */
  private def persisToDisk: Unit = {
    // TODO
  }

  /**
   * Go to .sit file and obtain the stored commit information
   * @return the commit list stored in .sit directory
   */
  private def readFromDisk: Commit = {
    // TODO
    null
  }

  /**
   * Given the commit message sent from user, calculate the diff in the directory
   * and prepend new commit to the list, this should result in a new head
   * @param commitMessage message specified by the user
   * @return the commit list stored in .sit file
   */
  def commit(commitMessage: String): Unit = {
    // TODO
  }

  /**
   * we apply all the changes to the file and calculate the diff
   * @return Diff object containing the information
   */
  private def diff(): Diff = {
    // TODO
    null
  }

  def revert(): Unit = {
    // TODO
  }

  /**
   * print out the full commit list
   */
  def history(): Unit = {
    // TODO
  }

}

object Sit {

  def init(projectPath: String): Sit = {
    val potentialPath: String = if (projectPath.endsWith("/")) {
      projectPath.concat(".sit")
    } else {
      projectPath.concat("/.sit")
    }

    if (Files.exists(Paths.get(potentialPath))) {
      throw SitExistException("This project has a Sit already")
    } else {
      val writer = new PrintWriter(new File(potentialPath))
      writer.write("\n") // a trivial write to make an empty .sit file
      writer.close()
      new Sit(None, potentialPath)
    }
  }

  def main(args: Array[String]): Unit = {

  }
}

case class SitExistException(msg: String) extends Exception(msg)
