import sbt.IO

import java.io.File
import java.net.URL
import java.nio.file.{Files, Path, StandardOpenOption}
import java.security.MessageDigest

import sbt.{TaskKey, Compile}
import sbt.Keys.{fullClasspathAsJars,target}

object CustomUtil {

  val sha1digester: MessageDigest = MessageDigest.getInstance("SHA-1")

  def sha1hex(b: Array[Byte]): String =
    sha1digester.clone().asInstanceOf[MessageDigest].digest(b)
      .map { h => f"$h%02x" }.mkString

  def fetchResource(urlStr: String, sha1sum: String, target: Path): File = {
    Files.createDirectories(target.getParent)
    val url = new URL(urlStr)
    val fos = Files.newOutputStream(target, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    try IO.transferAndClose(url.openStream(), fos)
    finally fos.close()
    val csha1 = sha1hex(Files.readAllBytes(target))
    if (sha1sum != csha1) {
      Files.delete(target)
      throw new AssertionError(s"sha1 of »$urlStr« did not match »$sha1sum«")
    }

    target.toFile
  }

  val writeClasspath = TaskKey[Unit]("writeClasspath", "writes the classpath to a file in the target dir") := {
    val cp         = (Compile / fullClasspathAsJars).value
    val cpstring   = cp.map(at => s"""-cp "${at.data.toString.replace("\\", "/")}"\n""").mkString("")
    val targetpath = target.value.toPath.resolve("classpath.txt")
    IO.write(targetpath.toFile, cpstring)
    ()
  }

}
