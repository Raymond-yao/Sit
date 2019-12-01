import java.io.{File, FileInputStream, FileWriter, PrintWriter}

import Model.{Commit, Diff}
import java.nio.file.{Files, Path, Paths}
import java.util.{Optional, Properties, stream}
import java.util.UUID.randomUUID

import net.liftweb.json._
import net.liftweb.json.JsonAST.JObject

import scala.util.Try

class Sit(private val projectPath: String,
          private val sitConfigPath: String) {

  private val emptyDiff = Diff(Map.empty, Map.empty)

  private val (
    head: Option[Commit],
    tail: Option[Commit],
    baseProject: Properties) = Try(readFromDisk).getOrElse((None, None, new Properties()))
  private val newestProject = head.map(Util.rebuild(baseProject, _)).getOrElse(baseProject)

  private def buildJsonList(c: Commit): List[JObject] = {
    List(c.toJson) ++ c.parent.map(buildJsonList).getOrElse(List())
  }

  /**
   * persist the current Model.Commit list to the directory, ideally it should be located in ./.sit
   */
  private def persisToDisk(commit: Commit): Unit = {
    val fw = new FileWriter(sitConfigPath)
    fw.write(prettyRender(JArray(buildJsonList(commit))))
    fw.close()

  }

  /**
   * Go to .sit file and obtain the stored commit information
   *
   * @return (head, tail, baseProject)
   */
  private def readFromDisk: (Option[Commit], Option[Commit], Properties) = {
    // TODO
    val stub = Commit(randomUUID().toString, Some(
      Commit(randomUUID().toString, Some(
        Commit(randomUUID().toString, None, Diff(Map("a" -> "2", "b" -> "3"), Map()), "first commit", 100L)
      ), Diff(Map("c" -> "4"), Map("a" -> "2")), "second commit", 120L)),
      Diff(Map("d" -> "5"), Map("b" -> "3")), "third commit", 130L)
    (Some(stub), None, new Properties())
  }

  /**
   * Given the commit message sent from user, calculate the diff in the directory
   * and prepend new commit to the list, this should result in a new head
   *
   * @param commitMessage message specified by the user
   * @return the commit list stored in .sit file
   */
  def commit(commitMessage: String): Unit = {
    val currCommit = head match {
      case Some(commit) => commit.addNext(
        diff().getOrElse(emptyDiff),
        commitMessage)
      case None => Commit(
        randomUUID().toString,
        None,
        diff().getOrElse(emptyDiff),
        commitMessage,
        0)
    }
    persisToDisk(currCommit)
  }

  /**
   * we apply all the changes to the file and calculate the diff
   *
   * @return Diff object containing the information
   */
  private def diff(): Option[Diff] = {
    val file: stream.Stream[Path] = Files.list(Paths.get(projectPath))
    val fileInJavaOpt: Optional[Path] = file.filter((p: Path) => {
      p.getFileName.toString.endsWith(".properties")
    }).findFirst()

    // Since scala is not quite compatible with java 1.8 yet.
    // manually handle java.util.Optional to scala.Option
    val fileOpt: Option[Path] = if (fileInJavaOpt.isEmpty) None else Some(fileInJavaOpt.get())

    fileOpt.map { filePath =>
      val fileStream = new FileInputStream(filePath.toFile)
      val theProperty = new Properties()
      theProperty.load(fileStream)

      Util.compare(theProperty, newestProject)
    }
  }

  def getDiff(commitId: String, acc: Diff, currHead: Option[Commit]): Diff =
    currHead match {
      case None => acc;
      case Some(commit) => {
        commit match {
          case Commit(`commitId`, _, _, _, _) => acc // we have found the target we are reverting to
          case _ => {
           // commit.diff.deleted.foldLeft(acc.added, (b: Map[String, String], a: (String, String)) => b + (a._1 -> a._2))
           // commit.diff.added.foldLeft(acc.deleted, (b: Map[String, String], a: (String, String)) => b + (a._1 -> a._2))
            acc
          }
        }
      }
    }

  def revert(commitId: String): Unit = {
    var acc: Diff = Diff(Map(), Map())
    getDiff(commitId, acc, head)
    // TODO commit diff properly once commit is done
    commit(s"revert to ${commitId}")
  }

  private def histHelper(base: Properties, commit: Commit): Properties = {
    println(s"Commit: ${commit.id} ")
    println(s"    ${commit.commitMessage} ")
    println(s"TimeStamp: ${commit.timestamp}")
    commit.diff.added.foreach(pair => {
      println((s"+ ${pair._1} = ${pair._2}"))
    })
    commit.diff.deleted.foreach(pair => {
      println((s"- ${pair._1} = ${pair._2}"))
    })
    base
  }

  /**
   * print out the full commit list
   */

  def history(): Unit =
    head.map(h => h.foldLeft(histHelper, newestProject)).getOrElse(println("HEAD is null!"))
}

object Sit {

  def init(projectPath: String): Sit = {
    val sitPath: String = if (projectPath.endsWith("/")) {
      projectPath.concat(".sit")
    } else {
      projectPath.concat("/.sit")
    }

    if (!Files.exists(Paths.get(sitPath))) {
      val writer = new PrintWriter(new File(sitPath))
      writer.write("\n") // a trivial write to make an empty .sit file
      writer.close()
    }

    new Sit(projectPath, sitPath)
  }

  def main(args: Array[String]): Unit = {
    val sit = Sit.init("some/good/path")
    sit.commit("fourth commit")
  }
}

case class SitExistException(msg: String) extends Exception(msg)
