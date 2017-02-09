package xylophone

import language.dynamics
import scala.util.{Failure, Success}

case class Xml(elements: Seq[Xml.Node], path: Xml.SeqPath) extends Dynamic {
  
  import Xml._
   
  def selectDynamic(tag: String)(implicit namespace: Namespace): Xml =
    Xml(elements, Select(path, Name(namespace, tag)))
  
  def apply(index: Int = 0): XmlNode =
    XmlNode(elements, Index(path, index))
  

  def applyDynamic(name: String)(index: Int): XmlNode = XmlNode(elements, Index(path, index))


  override def toString(): String =
    $normalize.map(_.string(Set())).mkString("")

  def $normalize: Seq[Xml.Node] = Xml.normalize(elements, path)
    
}

case class XmlNode(elements: Seq[Xml.Node], path: Xml.NodePath) extends Dynamic {
  def apply(attribute: Symbol)(implicit namespace: Xml.Namespace): XmlAttribute =
    XmlAttribute(this, Xml.Name(namespace, attribute.name))
  
  def selectDynamic(tag: String)(implicit namespace: Xml.Namespace): Xml =
    Xml(elements, Xml.Select(path, Xml.Name(namespace, tag)))

  def * : Xml = Xml(elements, Xml.All(path))

  override def toString(): String = $normalize.map(_.string(Set())).mkString("")

  def $normalize: Option[Xml.Node] = Xml.normalize(elements, path).headOption
}

case class XmlAttribute(xmlNode: XmlNode, attributeName: Xml.Name) {
  def $normalize: Option[String] =
    xmlNode.$normalize.headOption.to[Seq].flatMap(_.attribute(attributeName)).headOption
}

object Xml {


  def parse(str: String)(implicit parser: Parser[String, Xml]): Xml = {
    parser.parse(str) match {
      case Success(parsedXml) => parsedXml
      case Failure(_) => throw ParseException(str)
    }
  }

  def normalize(elements: Seq[Node], path: Path): Seq[Xml.Node] = path match {
    case Root(xml) => xml
    case Select(path, name) => elements.collect { case element@Element(`name`, _, _) => element }
    case Index(path, index) => if(elements.length > index) List(elements(index)) else Nil
    case All(path) => normalize(elements.headOption.to[Seq].flatMap(_.children), path)
  }

  sealed trait Path
  sealed trait SeqPath extends Path
  sealed trait NodePath extends Path

  case class Root(xml: Seq[Node]) extends NodePath
  case class Select(path: Path, name: Name) extends SeqPath
  case class Index(path: SeqPath, index: Int) extends NodePath
  case class All(path: NodePath) extends SeqPath


  object Namespace {
    val XmlnsNamespace = Namespace("xmlns", None)
  }
  
  implicit object DefaultNamespace extends Namespace("", None) {
    override def prefix(tag: String): String = tag
  }

  case class Namespace(alias: String, namespace: Option[String]) {

    def prefix(tag: String): String = s"$alias:$tag"

    def xmlnsAttribute: Option[(Name, String)] = namespace.map { nsString =>
      (Name(Namespace.XmlnsNamespace, alias), nsString)
    }
  }

  case class Name(namespace: Namespace, name: String) {
    override def toString(): String = namespace.prefix(name)
  }

  sealed trait Node {
    def attribute(name: Name): Option[String]
    def children : Seq[Node]

    def string(namespaces: Set[Namespace]): String
  }

  case class Text(text: String) extends Node {
    def attribute(name: Name): Option[String] = None
    def children : Seq[Node] = Nil

    def string(namespaces: Set[Namespace]): String = text
  }

  case class Element(name: Name, attributes: Map[Name, String], children: Seq[Node]) extends Node {
    def attribute(name: Name): Option[String] = attributes.get(name)
    
    def string(namespaces: Set[Namespace]): String = {
      val (atts, nss) =
        if(namespaces contains name.namespace) (attributes ++ name.namespace.xmlnsAttribute, namespaces)
        else (attributes, namespaces)

      val attsString = if(atts.size > 0) atts.map { 
        case (name, value) => s"""$name="$value"""
      }.mkString(" ", " ", "") else ""
      
      val content = children.map { child => child.string(nss).mkString("") }

      s"<$name$attsString>$content</$name>"
    }
  }

  case class ProessingInstruction(target: String, content: String) extends Node {
    def attribute(name: Name): Option[String] = None
    def children : Seq[Node] = Nil

    def string(namespaces: Set[Namespace]): String = s"<! $target $content !>"
  }

  case class Comment(content: String) extends Node {
    def attribute(name: Name): Option[String] = None
    def children : Seq[Node] = Nil

    def string(namespaces: Set[Namespace]): String = s"<!--$content-->"
  }

  case class Entity(name: String) extends Node {
    def attribute(name: Name): Option[String] = None
    def children : Seq[Node] = Nil

    def string(namespaces: Set[Namespace]): String = s"&$name;"
  }
}
