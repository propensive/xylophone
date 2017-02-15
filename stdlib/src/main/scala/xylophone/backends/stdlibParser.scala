package xylophone.backends.stdlib

import xylophone._, Ast._

import scala.util.Try
import scala.xml.{Node => ScalaNode, Text => ScalaText, _}

private[stdlib] object StdLibXmlStringParser extends Parser[String] {

  def parse(str: String): Try[XmlSeq] =
    Try(Utility.trim(XML.loadString(str))).map(elem => XmlSeq(Seq(convert(elem)), Vector()))

  //TODO: remove recursion
  private def convert(elem: ScalaNode): Node = {
    val tagName = elem.label
    val attributes = extractAttributes(elem.attributes)
    val namespace = parseNamespace(elem)

    Element(Name(namespace, tagName), attributes, elem.child.map {
      case node: ScalaText => Text(node.text)
      case node => convert(node)
    })
  }

  private def extractAttributes(attrs: MetaData): Map[Name, String] = {
    
    def parseNamespace(str: String): Namespace = str.indexOf(':') match {
      case -1 => DefaultNamespace
      case n => Namespace(str.substring(0, n), Some(str.substring(n + 1)))
    }

    attrs.map { x => (Name(parseNamespace(x.prefixedKey), x.key), x.value.text) }.toMap
  }

  private def parseNamespace(node: ScalaNode): Namespace =
    if (node.prefix != null && node.prefix.nonEmpty) Namespace(node.prefix, None)
    else DefaultNamespace
}
