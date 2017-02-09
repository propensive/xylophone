package xylophone.stdlib

import xylophone.Xml.{DefaultNamespace, Element, Name, Namespace, Node => XmlNode, Text => XmlText}
import xylophone.{Parser, Xml}

import scala.util.Try
import scala.xml._


// TODO need to move to another project (xylophone-stdlib)
private[stdlib] object StdLibXmlStringParser extends Parser[String, Xml] {

  def parse(str: String): Try[Xml] = {
    Try(Utility.trim(XML.loadString(str))).map(elem => Xml(Seq(convert(elem)), Vector()))
  }

  //TODO: remove recursion
  private def convert(elem: Node): XmlNode = {
    val tagName = elem.label
    val attributes = extractAttributes(elem.attributes)
    val namespace = parseNamespace(elem)

    Element(Name(namespace, tagName), attributes,
      elem.child.map(node => if (node.isInstanceOf[Text]) XmlText(node.text) else convert(node)))
  }

  private def extractAttributes(attrs: MetaData): Map[Xml.Name, String] = {
    def parseNamespace(str: String): Namespace = {
      str.split(":") match {
        case Array(prefix, key) => Namespace(prefix, Some(key))
        case _ => DefaultNamespace
      }
    }
    attrs.map(x => (Name(parseNamespace(x.prefixedKey), x.key), x.value.text)).toMap
  }


  private def parseNamespace(node: Node): Namespace = {
    if (node.prefix != null && node.prefix.nonEmpty) Namespace(node.prefix, None)
    else DefaultNamespace
  }

}