package xylophone

import rapture.core.{MethodConstraint, Mode}

import language.dynamics
import language.experimental.macros
import scala.util.{Failure, Success}

/** represents a sequence of XML nodes, specified as a root sequence of AST nodes, and a path
 *  into that AST
 *
 *  This implements the [[scala.Dynamic]] trait, so may be dereferenced with a key of any
 *  alphanumeric name.
 *
 *  Note that both parameters are "hidden" by their names being prefixed with a '$', thus
 *  making it harder, but not impossible, to call them accidentally when intending to
 *  dereference the XML node sequence.
 *
 *  @param $root  the root of the XML node sequence, from which the path dereferences
 *  @param $path */
case class XmlSeq($root: Seq[Ast.Node], $path: Vector[Ast.Path]) extends Xml with Dynamic {
  import Ast._

  /** filters the sequence of nodes to only return those with matching tag name and namespace
   */
  def selectDynamic(tag: String)(implicit namespace: Namespace): XmlSeq =
    XmlSeq($root, $path :+ Filter(Name(namespace, tag)))

  /** returns the [[XmlNode]] at element number [[index]]
   *
   *  @param index  the index of the element number in the [[XmlSeq]] to access
   *  @return the [[XmlNode]] at index [[index]] */
  def apply(index: Int = 0): XmlNode =
    XmlNode($root, $path :+ Index(index))

  /** gets the joined sequence of all children of this sequence
   *
   *  This finds the children of all [[XmlNode]]s in this sequence, dropping any
   *  non-[[XmlNode]] elements, and returns them as a joined [[XmlSeq]].
   *
   *  @return the joined [[XmlSeq]] of all the nodes' children */
  def * : XmlSeq =
    XmlSeq($root, $path :+ Children)

  /** composes the [[selectDynamic]] method with the application of the [[apply]] method
   *
   *  @param tag        the tag name to dereference
   *  @param index      the index of the element to return, after dereferencing
   *  @param namespace  the namespace for the dereferencing tag name */
  def applyDynamic(tag: String)(index: Int = 0)(implicit namespace: Ast.Namespace): XmlNode =
    selectDynamic(tag)(namespace)(index)

  /** returns a string representation of this XML node */
  override def toString(): String =
    $normalize.map(_.string(Set())).mkString("")

  /** joins an [[XmlNode]] to the end of this [[XmlSeq]]
   *
   *  This is the equivalent of the `:+` method in the Scala collections library.
   *
   *  @param xmlNode  the XML node to append to the right of this
   *  @return the joined XML node sequence */
  def :~(xmlNode: XmlNode): XmlSeq = XmlSeq($normalize ++ xmlNode.$normalize, Vector())
  
  /** joins an [[XmlNode]] to the start of this [[XmlSeq]]
   *
   *  This is the equivalent of the `+:` method in the Scala collections library.
   *
   *  @param xmlNode  the XML node to append to the left of this
   *  @return the joined XML node sequence */
  def ~:(xmlNode: XmlNode): XmlSeq =
    XmlSeq(xmlNode.$normalize.to[Seq] ++ $normalize, Vector())
 
  /** joins two [[XmlSeq]]s together
   *
   *  This is the equivalent of the `++` method in the Scala collections library.
   *
   *  @param xmlSeq  the XML sequence to append to the right of this
   *  @return the joined XML sequence */
  def ~~(xmlSeq: XmlSeq): XmlSeq = XmlSeq($normalize ++ xmlSeq.$normalize, Vector())
  
  private[xylophone] def $normalize: Seq[Node] = Xml.normalize($root, $path)
}

object :~ {
  /** decomposes an [[XmlSeq]] into its `init` and `last` parts */
  def unapply(xmlSeq: XmlSeq): Option[(XmlSeq, XmlNode)] = xmlSeq.$normalize match {
    case init :+ last => Some((XmlSeq(init, Vector()), XmlNode(Seq(last), Vector())))
    case _ => None
  }
}

object ~: {
  /** decomposes an [[XmlSeq]] into its `head` and `tail` parts */
  def unapply(xmlSeq: XmlSeq): Option[(XmlNode, XmlSeq)] = xmlSeq.$normalize match {
    case head +: tail => Some((XmlNode(Seq(head), Vector()), XmlSeq(tail, Vector())))
    case _ => None
  }
}

object XmlSeq {
  /** serializes a value of type [[T]] to an [[XmlSeq]]
   *
   *  @param data        the value to serialize
   *  @param serializer  the implicit serializer for the type [[T]] */
  def apply[T](data: T)(implicit serializer: SeqSerializer[T]): XmlSeq =
    serializer.serialize(data)
  
  /** parses a String into an [[XmlSeq]] value
   *
   *  @param src   the source value to read from
   *  @param mode  the mode for mitigating errors
   *  @param parser  the implicit parser used to interpret the source */
  def parse(src: String)(implicit mode: Mode[_ <: MethodConstraint],
      parser: Parser[String]): mode.Wrap[XmlSeq, ParseException] = mode.wrap {

    val wrappedString = wrapXmlByTag(src)

    parser.parse(wrappedString) match {
      case Success(parsedXml) => XmlSeq(parsedXml.$normalize, Vector())
      case Failure(_) => mode.exception(ParseException(src))
    }
  }
  
  private[xylophone] val Empty = XmlSeq(Nil, Vector())
  private[xylophone] def apply(element: Ast.Node): XmlSeq = XmlSeq(Seq(element), Vector())
  private[xylophone] def apply(root: Seq[Ast.Node]): XmlSeq = XmlSeq(root, Vector())
  
  private[this] def wrapXmlByTag(str: String): String =
    if (str.trim.startsWith("<?xml"))  str.replace("?>", s"?>$TopOpenTag") + TopClosingTag
    else TopOpenTag + str + TopClosingTag
  
  private[this] val TopOpenTag = "<self>"
  private[this] val TopClosingTag = "</self>"
}
/** represents a single XML node, specified by a root sequence of elements, and a path into
 *  those elements, pointing to a single node */
case class XmlNode($root: Seq[Ast.Node], $path: Vector[Ast.Path]) extends Xml with Dynamic {
  import Ast._

  /** selects child nodes of with the tag name, [[tag]] in the namespace, [[namespace]]
   *
   *  @param tag        the name of the tag to access
   *  @param namespace  the namespace for the specified tag name
   *  @return the children of this node with the matching tag names and namespaces */
  def selectDynamic(tag: String)(implicit namespace: Namespace): XmlSeq =
    XmlSeq($root, $path :+ Select(Name(namespace, tag)))

  /** gets all children of this node */
  def * : XmlSeq = XmlSeq($root, $path :+ Children)
 
  /** composes dereferencing and indexing this [[XmlNode]]
   *
   *  @param tag        the name of the tags to dereference
   *  @param index      the element number to subsequently get
   *  @param namespace  the namespace for the tag name */
  def applyDynamic(tag: String)(index: Int = 0)(implicit namespace: Namespace): XmlNode =
    selectDynamic(tag)(namespace)(index)

  /** gets the attribute with the specified name in the given namespace */
  def apply(attribute: Symbol)(implicit namespace: Namespace): XmlAttribute =
    XmlAttribute(this, Name(namespace, attribute.name))

  /** provides a string representation of this node */
  override def toString(): String = $normalize.map(_.string(Set())).mkString("")

  /** joins this node to another node to produce an [[XmlSeq]] */
  def ~(xmlNode: XmlNode): XmlSeq = XmlSeq($normalize.to[Seq] ++ xmlNode.$normalize, Vector())
  
  private[xylophone] def $normalize: Option[Node] = Xml.normalize($root, $path).headOption
}

object XmlNode {

  private[xylophone] val Empty = XmlNode(Nil, Vector())
  private[xylophone] def apply(element: Ast.Node): XmlNode = XmlNode(Seq(element), Vector())
  private[xylophone] def apply(root: Seq[Ast.Node]): XmlNode = XmlNode(root, Vector())

  /** creates a new [[XmlNode]], serializing the value [[data]].
   *
   *  @param data  the value to serialize to XML
   *  @tparam T  the type of the value to serialize */
  def apply[T](data: T)(implicit serializer: NodeSerializer[T]): XmlNode =
    serializer.serialize(data)
}

trait Xml

object Xml {

  def serializer[T]: SeqSerializer[T] = macro XmlMacros.serializerMacro[T]

  private[xylophone] def normalize($root: Seq[Ast.Node],
                                   $path: Vector[Ast.Path]): Seq[Ast.Node] = {
    $path.foldLeft($root) {
      case (element, Ast.Filter(name)) =>
        element.filter {
          case elem@Ast.Element(n, _, _) => n == name
          case _ => false
        }

      case (element, Ast.Select(name)) =>
        element.collect { case element@Ast.Element(n, _, _) =>
          element.children.collect { case element@Ast.Element(`name`, _, _) =>
            element
          }
        }.flatten
      
      case (element, Ast.Index( index)) =>
        if(element.length > index) List(element(index)) else Nil

      case (element, Ast.Children) =>
        element.collect { case elem@Ast.Element(_, _, _) =>
          elem.children
        }.flatten
    }
  }
}

case class XmlAttribute(xmlNode: XmlNode, attributeName: Ast.Name) {
  
  private[xylophone] def $normalize: Option[String] =
    xmlNode.$normalize.to[Seq].flatMap(_.attribute(attributeName)).headOption

}
