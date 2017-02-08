package xylophone.stdlib

import xylophone.Xml.{Name, Namespace, Tag, Node => XmlNode, Text => XmlText}
import xylophone.{Parser, Xml}

import scala.util.Try
import scala.xml._


// TODO need to move to another project (xylophone-stdlib)
private[stdlib] object StdLibXmlStringParser extends Parser[String, Xml] {

  def parse(str: String): Try[Xml] = {
    Try(Utility.trim(XML.loadString(str))).map(elem => Xml(Seq(convert(elem))))
  }

  //TODO: remove recursion
  private def convert(elem: Node): XmlNode = {
    val tagName = elem.label
    val attributes = extractAttributes(elem.attributes)
    val namespace = parseNamespace(elem)

    Tag(Name(namespace, tagName), attributes,
      elem.child.map(node => if (node.isInstanceOf[Text]) XmlText(node.text) else convert(node)))
  }

  private def extractAttributes(attrs: MetaData): Map[Xml.Name, String] = {
    def parseNamespace(str: String): Namespace = {
      str.split(":") match {
        case Array(prefix, key) => Namespace(Some((key, prefix)))
        case _ => Namespace(None)
      }
    }
    attrs.map(x => (Name(parseNamespace(x.prefixedKey), x.key), x.value.text)).toMap
  }


  private def parseNamespace(node: Node): Namespace = {
    if (node.prefix != null && node.prefix.nonEmpty) Namespace(Some(("", node.prefix)))
    else Namespace(None)
  }

}