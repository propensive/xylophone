package xylophone

/** object providing types and for operating on the XML AST */
object Ast {

  /** represents an element of a path which operates on a [[Seq[XmlNode]]] */
  sealed trait Path

  /** path element representing selecting the child elements of the specified name */
  case class Select(name: Name) extends Path

  /** path element representing filtering onle the elements with the specified name */
  case class Filter(name: Name) extends Path
  
  /** path element representing selecting the element at the specified index */
  case class Index(index: Int) extends Path

  /** path element representing selecting all child elements */
  case object Children extends Path

  /** companion object to the [[Namespace]] type */
  object Namespace {
    
    /** representation of the default XML namespace */
    val XmlnsNamespace: Namespace = Namespace("xmlns", None)
  }

  implicit object DefaultNamespace extends Namespace("", None) {
    /** in the default namespace, there is no prefix */
    override def prefix(name: String): String = name
  }

  /** a representation of an XML namespace, consisting of an alias and optionally an absolute
   *  namespace value, which is typically in the form of a URL */
  case class Namespace(alias: String, namespace: Option[String]) {

    /** applies the namespace to a string
     *
     *  @param name  the name to be prefixed with this namespace */
    def prefix(name: String): String = if(alias.trim.isEmpty) name else s"$alias:$name"

    /** the attribute key and value for the XMLNS declaration of this namespace */
    def xmlnsAttribute: Option[(Name, String)] = namespace.map { nsString =>
      (Name(Namespace.XmlnsNamespace, alias), nsString)
    }
  }

  /** a name, being a key within an aliased namespace, used for tag and attribute names */
  case class Name(namespace: Namespace, name: String) {
    override def toString: String = namespace.prefix(name)
  }

  /** representation of a general XML AST node */
  sealed trait Node {
    def attribute(name: Name): Option[String] = None
    def string(namespaces: Set[Namespace]): String
  }

  /** an XML element node
   *
   *  @param name        the tag name (with namespace) of this element
   *  @param attributes  the unordered map of attributes, as mappings from [[Name]]s to
   *                     [[Strings]]
   *  @param children    the child nodes of this element */
  case class Element(name: Name,
                     attributes: Map[Name, String],
                     children: Seq[Node]) extends Node {

    /** returns the attribute matching the specified name, if it exists */
    override def attribute(name: Name): Option[String] = attributes.get(name)

    /** a string representation of this element, assuming the specified set of namespaces
     *  have been defined in a parent node */
    def string(namespaces: Set[Namespace]): String = {
      
      val allAttributes = if(namespaces contains name.namespace)
          attributes ++ name.namespace.xmlnsAttribute else attributes

      val attributesString = if(allAttributes.nonEmpty) allAttributes.map {
        case (key, value) => s"""$key="$value""""
      }.mkString(" ", " ", "") else ""

      val content = children.map(_.string(namespaces).mkString("")).mkString("")

      if(content != "") s"<$name$attributesString>$content</$name>"
      else s"<$name$attributesString/>"
    }
  }

  /** an XML text node */
  case class Text(text: String) extends Node {
    def string(namespaces: Set[Namespace]): String = text
    override def toString: String = string(Set())
  }

  /** an XML processing instruction node */
  case class ProessingInstruction(target: String, content: String) extends Node {
    def string(namespaces: Set[Namespace]): String = s"<! $target $content !>"
  }

  /** an XML comment node */
  case class Comment(content: String) extends Node {
    def string(namespaces: Set[Namespace]): String = s"<!--$content-->"
  }

  /** an XML entity node */
  case class Entity(name: String) extends Node {
    def string(namespaces: Set[Namespace]): String = s"&$name;"
  }
}
