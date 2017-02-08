package xylophone

import language.dynamics

case class Xml(elements: Seq[Xml.Node]) extends Dynamic {
  
  import Xml._
  
  def selectDynamic(tag: String)(implicit namespace: Namespace): Xml = {
    val TagName = Name(namespace, tag)
    Xml(elements.collect { case tag@Tag(TagName, _, _) => tag })
  }
  
  def apply(index: Int = 0): Node = elements(index)

  override def toString(): String = elements.mkString("")

}

object Xml {

  object Namespace {
    implicit val defaultNamespace: Namespace = Namespace(None)
  }
  
  case class Namespace(namespace: Option[(String, String)]) {
    
    override def equals(that: Any): Boolean = that match {
      case Namespace(ns) => ns.map(_._1) == namespace.map(_._1)
      case _ => false
    }

    override def hashCode: Int = namespace.map(_._1).hashCode
  }

  case class Name(namespace: Namespace, name: String) {
    override def toString(): String = namespace.namespace match {
      case Some((ns, alias)) => s"$alias:$name"
      case None => name
    }
  }

  case class Attribute(name: Name, value: String)

  sealed trait Node extends Dynamic {
    def apply(attribute: Symbol)(implicit namespace: Namespace): Option[Attribute]
    def selectDynamic(tag: String)(implicit namespace: Namespace): Xml
    def * : Xml
  }

  case class Text(text: String) extends Node {
    def apply(attribute: Symbol)(implicit namespace: Namespace): Option[Attribute] = None
    def * : Xml = Xml(Nil)
    def selectDynamic(tag: String)(implicit namespace: Namespace): Xml = Xml(Nil)

    override def toString(): String = text
  }

  case class Tag(name: Name, attributes: Map[Name, String], children: Seq[Node]) extends Node {
    def apply(attribute: Symbol)(implicit namespace: Namespace): Option[Attribute] = None
    def * : Xml = Xml(children)
    
    def selectDynamic(tag: String)(implicit namespace: Namespace): Xml = {
      val TagName = Name(namespace, tag)
      Xml(children.collect { case tag@Tag(TagName, _, _) => tag })
    }

    override def toString(): String = {
      val attsString = if(attributes.size > 0) attributes.map { case (k, v) =>
        s"""$k="$v""""
      }.mkString(" ", " ", "") else ""
      
      val content = children.mkString("")

      s"<$name$attsString>$content</$name>"
    }
  }

  case class ProessingInstruction(target: String, content: String) extends Node {
    def apply(attribute: Symbol)(implicit namespace: Namespace): Option[Attribute] = None
    def * : Xml = Xml(Nil)
    def selectDynamic(tag: String)(implicit namespace: Namespace): Xml = Xml(Nil)

    override def toString(): String = s"<! $target $content !>"
  }

  case class Entity(name: String) extends Node {
    def apply(attribute: Symbol)(implicit namespace: Namespace): Option[Attribute] = None
    def * : Xml = Xml(Nil)
    def selectDynamic(tag: String)(implicit namespace: Namespace): Xml = Xml(Nil)

    override def toString(): String = s"&$name;"
  }
}
