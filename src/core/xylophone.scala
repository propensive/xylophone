/*
    Xylophone, version 0.1.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package xylophone

import wisteria.*
import rudiments.*
import gossamer.*
import contextual.*

import annotation.targetName
import language.dynamics

case class Namespace(id: Text, uri: Text)

object XmlName:
  given Show[XmlName] = name => name.namespace match
    case Unset                => name.name
    case Namespace(prefix, _) => t"$prefix:${name.name}"

case class XmlName(name: Text, namespace: Maybe[Namespace] = Unset)

sealed trait Xml:
  def pointer: List[Int | Text | Unit]
  def root: Ast.Root
  
  @targetName("add")
  infix def +(other: Xml): Doc

  def string(using XmlPrinter[Text]): Text =
    summon[XmlPrinter[Text]].print(Doc(Ast.Root(Xml.normalize(this)*)))

type XmlPath = List[Text | Int | Unit]

object Xml:
  given Show[Xml] = xml =>
    try printers.compact.print(Doc(Ast.Root(Xml.normalize(xml)*)))
    catch case error: XmlAccessError => t"undefined"

  given (using XmlPrinter[Text]): clairvoyant.HttpResponse[Xml] with
    def mediaType: String = "application/xml; charset=utf-8"
    def content(xml: Xml): LazyList[IArray[Byte]] = LazyList(summon[XmlPrinter[Text]].print(xml).bytes)

  def print(xml: Xml)(using XmlPrinter[Text]): Text = summon[XmlPrinter[Text]].print(xml)

  def pathString(path: XmlPath): Text = if path.isEmpty then t"/" else path.map {
    case idx: Int   => t"[$idx]"
    case label: Text => t"/$label"
    case unit: Unit => t"/*"
    case _          => throw Impossible("should never match")
  }.join

  def parse(content: Text): Doc =
    import org.w3c.dom as owd, owd.Node.*
    import org.xml.sax as oxs
    import javax.xml.parsers.*
    import java.io.*

    val factory = DocumentBuilderFactory.newInstance().nn
    factory.setNamespaceAware(true)
    val builder = factory.newDocumentBuilder().nn

    val root = 
      try builder.parse(ByteArrayInputStream(content.bytes.unsafeMutable)).nn
      catch case e: oxs.SAXParseException =>
        throw XmlParseError(e.getLineNumber - 1, e.getColumnNumber - 1)

    def getNamespace(node: owd.Node): Maybe[Namespace] =
      Option(node.getPrefix) match
        case None         => Unset
        case Some(prefix) => Option(node.getNamespaceURI) match
                               case None      => Unset
                               case Some(uri) => Namespace(Text(prefix.nn), Text(uri.nn))

    def readNode(node: owd.Node): Ast = node.getNodeType match
      case CDATA_SECTION_NODE =>
        Ast.CData(Text(node.getTextContent.nn))

      case COMMENT_NODE =>
        Ast.Comment(Text(node.getTextContent.nn))

      case ELEMENT_NODE =>
        val xmlName = XmlName(Text(node.getLocalName.nn), getNamespace(node))
        val childNodes = node.getChildNodes
        
        val children =
          (0 until childNodes.nn.getLength).map(childNodes.nn.item(_).nn).map(readNode(_))
        
        val atts = (0 until node.getAttributes.nn.getLength).map(node.getAttributes.nn.item(_).nn)
        val attributes = atts.map { att =>
          val alias: Maybe[Namespace] = getNamespace(att)
          XmlName(Text(att.getLocalName.nn), alias) -> Text(att.getTextContent.nn)
        }.to(Map)
        
        Ast.Element(xmlName, children, attributes)

      case PROCESSING_INSTRUCTION_NODE =>
        val name = Text(node.getNodeName.nn)
        val content = Text(node.getTextContent.nn)
        Ast.ProcessingInstruction(name, content)

      case TEXT_NODE =>
        Ast.Textual(Text(node.getTextContent.nn))
      
      case id =>
        Ast.Comment(t"unrecognized node $id")

    readNode(root.getDocumentElement.nn) match
      case elem@Ast.Element(_, _, _, _) => Doc(Ast.Root(elem))
      case _                            => throw Impossible("xylophone: malformed XML")

  def normalize(xml: Xml): Seq[Ast] =
    def recur(path: XmlPath, current: Seq[Ast]): Seq[Ast] = path match
      case Nil =>
        current

      case (idx: Int) :: tail =>
        if current.length <= idx then throw XmlAccessError(idx, path)
        recur(tail, Seq(current(idx)))

      case (unit: Unit) :: tail =>
        val next = current
          .collect { case e@Ast.Element(_, children, _, _) => children }
          .flatten
          .collect { case e: Ast.Element => e }

        recur(tail, next)

      case (label: Text) :: tail =>
        val next = current
          .collect { case e@Ast.Element(_, children, _, _) => children }
          .flatten.collect { case e: Ast.Element if e.name.name == label => e }

        recur(tail, next)
      
      case _ :: tail =>
        throw Impossible("should never match")

    try recur(xml.pointer, xml.root.content)
    catch case err: XmlAccessError =>
      throw XmlAccessError(err.index, xml.pointer.dropRight(err.path.length))


case class Fragment(head: Text | Unit, path: XmlPath, root: Ast.Root)
extends Xml, Dynamic:
  def apply(idx: Int = 0): XmlNode = XmlNode(idx, head :: path, root)
  def attribute(key: Text): Attribute = apply(0).attribute(key)
  def pointer: XmlPath = (head :: path).reverse
  def selectDynamic(tagName: String): Fragment = Fragment(Text(tagName), head :: path, root)
  def applyDynamic(tagName: String)(idx: Int = 0): XmlNode = selectDynamic(tagName).apply(idx)
  
  @targetName("all")
  def * : Fragment = Fragment((), head :: path, root)
  
  @targetName("add")
  infix def +(other: Xml): Doc =
    Doc(Ast.Root(Xml.normalize(this) ++ Xml.normalize(other)*))
  
  def as[T](using reader: XmlReader[T]): T = apply().as[T]

case class XmlNode(head: Int, path: XmlPath, root: Ast.Root) extends Xml, Dynamic:
  def selectDynamic(tagName: String): Fragment = Fragment(Text(tagName), head :: path, root)
  def applyDynamic(tagName: String)(idx: Int = 0): XmlNode = selectDynamic(tagName).apply(idx)
  def attribute(attribute: Text): Attribute = Attribute(this, attribute)
  def pointer: XmlPath = (head :: path).reverse
  
  @targetName("all")
  def * : Fragment = Fragment((), head :: path, root)
  
  @targetName("add")
  infix def +(other: Xml): Doc =
    Doc(Ast.Root(Xml.normalize(this) ++ Xml.normalize(other)*))

  def as[T: XmlReader]: T =
    summon[XmlReader[T]].read(Xml.normalize(this)).getOrElse(throw XmlReadError())

case class Doc(root: Ast.Root) extends Xml, Dynamic:
  def pointer: XmlPath = Nil
  def selectDynamic(tagName: String): Fragment = Fragment(Text(tagName), Nil, root)
  def applyDynamic(tagName: String)(idx: Int = 0): XmlNode = selectDynamic(tagName).apply(idx)
  
  @targetName("all")
  def * : Fragment = Fragment((), Nil, root)
  
  @targetName("add")
  infix def +(other: Xml): Doc =
    Doc(Ast.Root(Xml.normalize(this) ++ Xml.normalize(other)*))

  def as[T: XmlReader]: T =
    summon[XmlReader[T]].read(Xml.normalize(this)).getOrElse(throw XmlReadError())

case class Attribute(node: XmlNode, attribute: Text):
  def as[T: XmlReader]: T =
    val attributes = Xml.normalize(node).headOption match
      case Some(Ast.Element(_, _, attributes, _)) => attributes
      case _                                      => throw XmlReadError()

    summon[XmlReader[T]]
      .read(Seq(Ast.Element(XmlName(t"empty"), Seq(Ast.Textual(attributes(XmlName(attribute)))))))
      .getOrElse(throw XmlReadError())

case class xmlAttribute() extends StaticAnnotation
case class xmlLabel(name: String) extends StaticAnnotation

extension [T](value: T)
  def xml(using writer: XmlWriter[T]): Doc = Doc(Ast.Root(writer.write(value)))
