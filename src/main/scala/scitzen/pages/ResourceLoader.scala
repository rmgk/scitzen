package scitzen.pages

import java.net.{JarURLConnection, URL}

import better.files._

import scala.collection.JavaConverters._
import scala.util.Try

class ResourceLoader() {

  val classloader: ClassLoader = getClass.getClassLoader
  val urls       : Seq[URL]    = classloader.getResources("META-INF/resources/webjars/").asScala.toList

  val assets: Seq[String] = urls.flatMap { url =>
    val jarUrlConnection = url.openConnection.asInstanceOf[JarURLConnection]
    jarUrlConnection.getJarFile.entries.asScala.filterNot(_.isDirectory).map(_.getRealName)
  }

  def findAsset(path: String): Option[String] = {
    assets.find(_.endsWith(path))
  }

  def resourceBytes(path: String): Iterator[Byte] = {
    Try {
      Resource.getAsStream(findAsset(path).get).buffered.bytes
    }.orElse(Try {
      Resource.getUrl()
      val resourcepath = Resource.getUrl()
      (File(resourcepath) / s"../../web/sass/main/stylesheets/$path").bytes
    }).get
  }

}
