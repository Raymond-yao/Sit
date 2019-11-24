import java.io.{File, FileInputStream, PrintWriter}

import Model.{Commit, Diff}
import java.nio.file.{Files, Path, Paths}
import java.util.UUID.randomUUID
import java.util.{Optional, Properties, stream}

import scala.util.Try
import scala.collection.immutable.List

class Sit(private val projectPath: String,
          private val sitConfigPath: String
         ) {

  private val (
    head: Option[Commit],
    tail: Option[Commit],
    baseProject: Properties) = Try(readFromDisk).getOrElse((None, None, new Properties()))
  private val newestProject = head.map(Util.rebuild(baseProject, _)).getOrElse(baseProject)

  /**
   * persist the current Model.Commit list to the directory, ideally it should be located in ./.sit
   */
  private def persisToDisk: Unit = {
    // TODO
  }

  /**
   * Go to .sit file and obtain the stored commit information
   *
   * @return (head, tail, baseProject)
   */
  private def readFromDisk: (Option[Commit], Option[Commit], Properties) = {
    // TODO
    val stub = Commit(
      randomUUID().toString,
      None,
      Diff(Map("a" -> "b", "c" -> "d"), Map()),
      "initial commit",
      0)

    val theHead = stub.addNext(
      Diff(Map("a" -> "kk", "something" -> "new", "cpp" -> "def"), Map("a" -> "b")),
      "make some modification"
    )

    (Some(theHead), Some(stub), new Properties())
  }

  /**
   * Given the commit message sent from user, calculate the diff in the directory
   * and prepend new commit to the list, this should result in a new head
   *
   * @param commitMessage message specified by the user
   * @return the commit list stored in .sit file
   */
  def commit(commitMessage: String): Unit = {
    // TODO
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

  def revert(): Unit = {
    // TODO
  }

  /**
   * print out the full commit list
   */
  def history(): Unit = {
    // TODO replace stub with real commit
    val stub = Commit(
      randomUUID().toString,
      None,
      Diff(Map("a" -> "b", "c" -> "d"), Map()),
      "initial commit",
      0)
    println(s"Commit: ${stub.id} ")
    println(s"    ${stub.commitMessage} ")
    println(s"TimeStamp: ${stub.timestamp}")
    stub.diff.added.foreach(pair => {
      println((s"+ ${pair._1} = ${pair._2}"))
    })
    stub.diff.deleted.foreach(pair => {
      println((s"- ${pair._1} = ${pair._2}"))
      })
    if (stub.parent != None) {
      // TODO print parent;
    }
  }

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
    val sit = Sit.init("C:/Users/62442/Documents/University/materials/CPSC 311/project/playground")
    // try put breakpoints in diff and run debugger to see the effect
    sit.diff()
    sit.history()
  }
}

case class SitExistException(msg: String) extends Exception(msg)
