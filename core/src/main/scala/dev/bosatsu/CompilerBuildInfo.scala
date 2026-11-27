package dev.bosatsu

/** How the running compiler was built.
  *
  * sbt's generated `BuildInfo` holds the version and git commit, so it changes
  * with every commit. Only the program entry points (`cli`, `cliJS`, `jsapi`
  * and `jsui`) generate it, and they pass it in, usually through
  * [[PlatformIO.buildInfo]]. Everything else depends on this trait, so a new
  * commit doesn't change the compiled output of the other projects and sbt's
  * build cache can reuse it.
  */
trait CompilerBuildInfo {
  def version: String
  def scalaVersion: String
  def gitHeadCommit: Option[String]

  /** Expected hash of the c runtime archive, or empty if the build didn't set
    * one.
    */
  def cRuntimeArchiveHash: String
}

object CompilerBuildInfo {

  /** Fixed values for code that has no entry point to supply real ones, such as
    * tests. It never changes, so depending on it doesn't invalidate builds.
    */
  val constant: CompilerBuildInfo =
    new CompilerBuildInfo {
      val version: String = "0.0.0-constant"
      val scalaVersion: String = "unknown"
      // All zeros, so commands that look up the c runtime by git sha have one
      // to use.
      val gitHeadCommit: Option[String] = Some("0" * 40)
      val cRuntimeArchiveHash: String = ""
    }
}
