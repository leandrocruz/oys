//> using dep "com.github.pathikrit::better-files:3.9.2"
//> using dep "dev.zio::zio:2.0.13" 
//> using dep "com.lihaoyi::mainargs:0.5.0"
//> using dep "dev.zio::zio-config:4.0.0-RC16"
//> using dep "dev.zio::zio-config-typesafe:4.0.0-RC16"
//> using dep "dev.zio::zio-config-magnolia:4.0.0-RC16"
//> using dep "org.eclipse.jgit:org.eclipse.jgit:6.5.0.202303070854-r"

import zio.*
import zio.Console.*
import io.AnsiColor._

object domain {

  import better.files.*
  import File.*

  case class GlobalConfig(quiet: Boolean)
  
  case class Modules(inclusions: Seq[String], exclusions: Seq[String])

  case class LocalConfig(name: String, root: String, modules: Modules) {
    def roots: Task[Seq[File]] = {
      ZIO.attempt {
        val exclusions = modules.exclusions.map(resolve)
        val inclusions = modules.inclusions.map(resolve) 
        File(root)
          .list
          .filter(_.isDirectory)
          .filterNot(_.name.startsWith("."))
          .filterNot(exclusions.contains)
          .toSeq ++ inclusions
      }
    }

    def resolve(path: String): File = {
      if      (path.startsWith("/")) File(path)
      else if (path.startsWith("~")) home / path.substring(2)
      else                           File(root) / path
    }
  }

  sealed trait Command {
    def execute(global: GlobalConfig, local: LocalConfig): Task[String]
  }
}

object git {

  import table.*
  import domain.*
  import mainargs.*
  import better.files.*
  import sys.process.*

  @main("git options")
  case class GitOptions(
    @arg(doc = "show project status for each module")
    status: Flag
  )

  case class GitRepo(name: String, file: File, branch: String, commit: String, status: String)

  case class GitCommand(options: GitOptions) extends Command {

    override def execute(global: GlobalConfig, local: LocalConfig): Task[String] = {
      def status: Task[String] = {

        def toString(repositories: Seq[GitRepo]): String = {

          def toRow(repo: GitRepo) = Row(Seq(repo.commit, repo.status, repo.name, repo.branch))
          def byName(r1: GitRepo, r2: GitRepo): Boolean = (r1.name compareTo r2.name) < 0
          def byBranch(r1: GitRepo, r2: GitRepo): Boolean = {
            
            def indexOf(name: String): Int = {
              name match
                case "master"  => 1
                case "develop" => 2
                case name if name.startsWith("feature") => 3
                case name if name.startsWith("hotfix")  => 4
                case name if name.startsWith("release") => 5
                case _ => 0
            }
            
            indexOf(r1.branch) < indexOf(r2.branch)
          }          

          val rows = repositories.sortWith((r1, r2) => {
            if(r1.branch == r2.branch) byName(r1, r2) else byBranch(r1, r2)
          }).map(toRow)
                    
          def render(row: Int, col: Column, value: String): Cell = {
            
            def branchColor(name: String): String = {
              name match {
                case "master"                            => CYAN
                case "develop"                           => BLUE
                case name if name.startsWith("feature/") => GREEN
                case name if name.startsWith("hotfix/")  => YELLOW
                case name if name.startsWith("release/") => YELLOW
                case name                                => MAGENTA
              }
            }

            def statusColor(name: String): String = if(name != "clean") RED else WHITE

            if(row == 0) {
              Cell(value).colored(BOLD)
            } else {
              col.name match {
                case "BRANCH" => Cell(value).colored(branchColor(value))
                case "STATUS" => Cell(value).colored(statusColor(value))
                case _        => Cell(value)
              }
            }
          }

          Table(
            cols       = Seq(Column("COMMIT"), Column("STATUS"), Column("NAME"), Column("BRANCH")),
            rows       = rows.toSeq,
            renderCell = render
          ).render()
        }

        def isGitRepo(dir: File): Boolean = (dir / ".git").exists

        def gitInfo(dir: File): Task[GitRepo] = {
          for {
            branch  <- run(dir, "git rev-parse --abbrev-ref HEAD").map(_.trim())
            commit  <- run(dir, "git rev-parse --short HEAD").map(_.trim())
            changes <- run(dir, "git status --porcelain").map(_.trim())
          } yield GitRepo(dir.name, dir, branch, commit, if(changes.isEmpty()) "clean" else "dirty")
        }

        for {
          roots      <- local.roots
          candidates <- ZIO.attempt(roots.filter(isGitRepo))
          repos      <- ZIO.foreachPar(candidates)(gitInfo)
        } yield toString(repos)
      }

      if(options.status.value) status
      else ZIO.fail(new Exception("Please specify an action"))
    }
  }

  def run(dir: File, cmd: String): Task[String] = {
    ZIO.attempt(Process(cmd, dir.toJava).!!)
       .mapError(e => new Exception(s"Error running command '$cmd'", e))
  }
}

object project {

  import better.files.*
  import File.*
  import domain.*
  import mainargs.*

  @main(name = "project options")
  case class ProjectOptions(
    @arg(doc = "describe project modules")
    describe: Flag
  )

  case class ProjectCommand(options: ProjectOptions) extends Command {
    override def execute(global: GlobalConfig, local: LocalConfig): Task[String] = {
      def describe = {
        def toString(inclusions: Seq[File], exclusions: Seq[File]): String = {
          def printFile(f: File): String = {
            if(f.exists) s" - ${f.canonicalPath} ${GREEN}FOUND${RESET}" 
            else         s" - ${f.canonicalPath} ${RED}MISSING${RESET}"
          }
          s"""${BOLD}${local.name}${RESET} ${BLUE}(root: ${local.root})${RESET}
             |
             |Inclusions:
             |${inclusions.map(printFile).mkString("\n")}
             |
             |Exclusions:
             |${exclusions.map(printFile).mkString("\n")}
             |
             |""".stripMargin
        }

        for {
          inc <- ZIO.attempt(local.modules.inclusions.map(local.resolve))
          exc <- ZIO.attempt(local.modules.exclusions.map(local.resolve))
        } yield toString(inc, exc)

      }
      
      if(options.describe.value) describe
      else ZIO.fail(new Exception("Please specify an action"))
    }
  }
}

object cfg {

  import domain.*
  import better.files.*
  import File.*
  import zio.config.*
  import zio.config.magnolia.*
  import zio.config.typesafe.*

  object GlobalConfig {
    val layer = ZLayer {
      val file = home / ".oysconfig"
      TypesafeConfigProvider
      .fromHoconFile(file.toJava, enableCommaSeparatedValueAsList = true)
      .load(deriveConfig[GlobalConfig])
    }
  }

  case class ConfigLoader(global: GlobalConfig) {
    def load: Task[LocalConfig] = {
      
      def loadFrom(root: File): Task[File] = {
        val file = root / ".oys"
        if(file.exists) 
          ZIO.succeed(file)
        else 
          root.parentOption match {
            case Some(parent) => loadFrom(root.parent)
            case None         => ZIO.fail(new Exception("Can't find project configuration. Please run this command from a project root"))
          }
      }
      
      def parse(file: File): Task[LocalConfig] = {
        TypesafeConfigProvider
          .fromHoconFile(file.toJava, enableCommaSeparatedValueAsList = true)
          .load(deriveConfig[LocalConfig])
      }

      for {
        file   <- loadFrom(File("."))
        config <- parse(file)
      } yield config.copy(root = file.parent.toJava.getAbsolutePath())
    }
  }

  object ConfigLoader {
    val layer = ZLayer.fromFunction(ConfigLoader.apply)
  }
}

object app {

  import cfg.*
  import domain.*
  import project.*
  import git.*
  import mainargs.*
  import zio.Console.*

  object Oys {
    val layer = ZLayer.fromFunction(Oys.apply)
  }

  case class Oys(global: GlobalConfig, loader: ConfigLoader) {
    def run(args: Seq[String]) = {

      def commandName(args: Seq[String]): Task[String] = {
        if(args.nonEmpty) ZIO.succeed(args(0))
        else              ZIO.fail(new Exception("No command name provided"))
      }

      def commandGiven(name: String, args: Seq[String]): Task[Command] = {
        
        def project = for {
          cfg <- ZIO.attempt(ParserForClass[ProjectOptions].constructOrThrow(args))
        } yield ProjectCommand(cfg)

        def git = for {
          cfg <- ZIO.attempt(ParserForClass[GitOptions].constructOrThrow(args))
        } yield GitCommand(cfg)

        name match {
          case "project" => project
          case "git"     => git
          case _         => ZIO.fail(new Exception(s"Unknown command '$name'"))
        }
      }

      for {
        local   <- loader.load
        name    <- commandName(args)
        command <- commandGiven(name, args.drop(1))
        result  <- command.execute(global, local)
        _       <- printLine(result)
      } yield ()
    }
  }
}

object table {

  case class Cell(value: String, color: Option[String] = None) {
    def colored(color: String) = this.copy(color = Some(color))
  }

  type CellRender = (Int, Column, String) => Cell

  case class Column(name: String)
  case class Row(values: Seq[String])

  case class Table(cols: Seq[Column], rows: Seq[Row], renderCell: CellRender) {
    
    def render(): String = {
      
      def sizeOf(col: Column): Int = {
        cols.zipWithIndex.map { case (c, index) => if(c == col) index else -1 } find { _ >= 0 } match {
          case None      => 20
          case Some(idx) => rows.map(row => row.values(idx)).map(_.length()).max
        }
      }
      
      def header: Seq[String] = cols.map(_.name)
      def renderRow(index: Int, values: Seq[String], sizes: Map[Column, Int]): String = {
        
        val byNumber: Map[Int, Column] = cols.zipWithIndex.toMap.map(_.swap)
        val cells: Seq[String] = values.zipWithIndex.map {
          case (value, n) => 
            val col = byNumber(n)
            val cell = renderCell(index, col, value)
            val text = cell.value.padTo(sizeOf(col), ' ')
            cell.color.map(color => s"${color}$text${RESET}").getOrElse(text)
        }
        " " + cells.mkString("    ") //•
      }

      val sizes = cols.map(col => (col, sizeOf(col))).toMap
      val data = Seq(renderRow(0, header, sizes)) ++
      rows.zipWithIndex.map {
        case (row, index) => renderRow(index + 1, row.values, sizes)
      }

      data.mkString("\n")
    }
  }
}

object oys extends ZIOAppDefault { 

  import cfg.*
  import app.*

  override def run = 
    for {
      args   <- getArgs
      result <- ZIO
        .serviceWithZIO[Oys](_.run(args))
        .provide(
          GlobalConfig.layer,
          ConfigLoader.layer,
          Oys.layer
        )
    } yield result
}