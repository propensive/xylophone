package xylophone

import contextual._

/** defines various parsing contexts which describe each possible position within the
 *  interpolated string that a substitution may be made. */
private[xylophone] object XmlContextTypes {
  /** provides a type-level representation of the context of any character of the parsed parts
   *  of the interpolated string. */
  sealed trait ContextType extends Context

  /** the attribute value, inside the quotes */
  case object AttributeValue extends ContextType

  /** the context in the name of the tag */
  case object TagName extends ContextType

  /** context after the '/' which terminates a tag name, indicating it is a self-closing tag */
  case object SelfClosingTag extends ContextType

  /** context after the '/' in a tag body which indicates the start of the end */
  case object TagClose extends ContextType

  /** context in the closing tag name */
  case object ClosingTagName extends ContextType

  /** context of the attribute name */
  case object AttributeName extends ContextType

  /** in the body of a tag */
  case object TagBody extends ContextType

  /** context after the '=' in an attribute value */
  case object AttributeEquals extends ContextType
  
  /** XML body content, outside of any tags, processing instructions or entities */
  case object Body extends ContextType

  /** in an XML entity reference within XML body */
  case object BodyEntity extends ContextType

  /** in an XML entity reference within an XML attribute */
  case object AttributeEntity extends ContextType
}
