import java.io.{File, FileInputStream, FileWriter, PrintWriter}

import Model.{Commit, Diff}
import java.nio.file.{Files, Path, Paths}
import java.util.{Optional, Properties, Scanner, stream}

import net.liftweb.json._

import scala.util.Try
import Console.{GREEN, RED, RESET}

class Sit(private val projectPath: String,
          private val sitConfigPath: String) {

  private val emptyDiff = Diff(Map.empty, Map.empty)

  private val (
    head: Option[Commit],
    baseProject: Properties) = Try(readFromDisk).getOrElse((None, new Properties()))
  private val newestProject = head.map(Util.rebuild(baseProject, _)).getOrElse(baseProject)

  /**
   * persist the current Model.Commit list to the directory, ideally it should be located in ./.sit
   */
  private def persisToDisk(commit: Commit): Unit = {
    val fw = new FileWriter(sitConfigPath)
    fw.write(prettyRender(JArray(Util.buildJsonList(commit))))
    fw.close()

  }

  /**
   * Go to .sit file and obtain the stored commit information
   *
   * @return (head, tail, baseProject)
   */
  private def readFromDisk: (Option[Commit], Properties) = {
    val reader = new Scanner(new File((sitConfigPath)))
    val sb: StringBuilder = new StringBuilder()
    while (reader.hasNextLine) {
      sb.append(reader.nextLine())
    }
    val json = sb.toString()
    val commitList: List[JValue] = parse(json).asInstanceOf[JArray].arr
    val headParsed = Util.rebuildCommitList(commitList)

    (headParsed, new Properties())
  }

  /**
   * Given the commit message sent from user, calculate the diff in the directory
   * and prepend new commit to the list, this should result in a new head
   *
   * @param commitMessage message specified by the user
   * @return the commit list stored in .sit file
   */
  def commit(commitMessage: String): Unit = {
    val finalDiff = diff().getOrElse(emptyDiff)

    val currCommit = head.map(
      _.addNext(finalDiff, commitMessage)
    ).getOrElse(Commit.head(finalDiff, commitMessage))

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

  private def buildMsg(msg: Array[String]): String = {
    val sb = new StringBuilder()
    var i = 1
    while (i < msg.length) {
      sb.append(msg(i))
      sb.append(" ")
      i += 1
    }

    sb.toString()
  }

  def main(args: Array[String]): Unit = {
    val projPath = args(0)
    val scanner = new Scanner(System.in);
    System.out.println("Sit Project");

    while (true) {
      System.out.print(">")
      Console.flush()
      val command = scanner.nextLine()
      val commands = command.split(" ")
      commands(0) match {
        case "init" =>
          Sit.init(projPath)
        case "commit" =>
          val commitMsg = if (commands.length < 2) {
            "empty message"
          } else {
            buildMsg(commands)
          }
          val sit = Sit.init(projPath)
          sit.commit(commitMsg)
          System.out.println("\n Committed")
        case "diff" =>
          val sit = Sit.init(projPath)
          System.out.println("\nShowing the current difference:")
          sit.diff().foreach(d => {
            System.out.println("Added:")
            d.added.foreach {
              case (k , v) =>
                System.out.println(s"   ${RESET}${GREEN}${k} -> ${v}${RESET}")
            }

            System.out.println("Deleted:")
            d.deleted.foreach {
              case (k , v) =>
                System.out.println(s"   ${RESET}${RED}${k} -> ${v}${RESET}")
            }
          })
        case "exit" =>
          return
        case _ =>
          System.out.println("\nUnknown Command")
      }
    }
  }
}

case class SitExistException(msg: String) extends Exception(msg)
