package scitzen.bibliography

import scitzen.parser.Biblet

trait BibtexProvider {
  def lookup(uri: String): Option[Biblet]
}
