package xylophone

import Ast._

import scala.util.Try
import scala.xml.{Node => ScalaNode, Text => ScalaText, _}

private[xylophone] object StdLibXmlStringParser extends Parser[String] {

  /** implements the XML parsing API by wrapping the Scala standard library API */
  def parse(str: String): Try[XmlSeq] =
    Try(Utility.trim(XML.loadString(str))).map { elem =>
      val Element(_, _, children) = convert(elem)
      val xmlSeq = XmlSeq(children, Vector())
      xmlSeq
    }

  private[this] def convert(elem: ScalaNode): Node = {
    val attributes = extractAttributes(elem.attributes)
    val namespace = parseNamespace(elem)

    Element(Name(namespace, elem.label), attributes, elem.child.map {
      case node: ScalaText => Text(node.text)
      case node => convert(node)
    })
  }

  private[this] def extractAttributes(attrs: MetaData): Map[Name, String] = {
    
    def parseNamespace(str: String): Namespace = str.indexOf(':') match {
      case -1 => DefaultNamespace
      case n => Namespace(str.substring(0, n), Some(str.substring(n + 1)))
    }

    attrs.map { x => (Name(parseNamespace(x.prefixedKey), x.key), x.value.text) }.toMap
  }

  private[this] def parseNamespace(node: ScalaNode): Namespace =
    if (node.prefix != null && node.prefix.nonEmpty) Namespace(node.prefix, None)
    else DefaultNamespace
}
