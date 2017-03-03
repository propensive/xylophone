package xylophone

import contextual._

/** implements the parsing logic for interpreting an `xml""` interpolated string, and the
 *  substitutions within it. */
object XmlInterpolator extends Interpolator with XmlInterpolator_1 {

  /** defines how [[XmlSeq]]s may be embedded in [[Body]] context */
  implicit val embedXmlSeqs = XmlInterpolator.embed[XmlSeq](
    Case(Body, Body) { x => XmlLike(x) }
  )

  /** defines how [[String]]s may be embedded in different contexts */
  implicit val embedStrings = XmlInterpolator.embed[String](
    Case(AttributeEquals, TagBody) { s => StringLike('"'+s+'"') },
    Case(AttributeValue, AttributeValue) { s => StringLike(s) },
    Case(Body, Body) { s => StringLike(s) }
  )

  /** defines how [[XmlNode]]s may be embedded in [[Body]] context */
  implicit val embedXmlNodes = XmlInterpolator.embed[XmlNode](
    Case(Body, Body) { x => XmlLike(XmlSeq(x.$root, x.$path)) }
  )

  /** defines how [[Pairs]]s of [[String]]s may be embedded as attributes in a tag */
  implicit val embedPairs = XmlInterpolator.embed[(String, String)](
    Case(TagBody, TagBody) { p => StringLike(s""" ${p._1}="${p._2}" """) }
  )
  
  /** defines how a [[Map[String, String]]] may be embedded as attributes in a tag */
  implicit val embedStringMap = XmlInterpolator.embed[Map[String, String]](
    Case(TagBody, TagBody) { m =>
      StringLike(m.map { case (k, v) => k+"="+'"'+v+'"' }.mkString(" ", " ", " ")) }
  )

  /** the type to which valid substitutions should be converted
   *
   *  The [[Input]] type determines how input values, from substitutions, may be passed into
   *  the interpolator. Whilst accepting values of potentially many different types, some of
   *  them not yet known, we need to present these as types which are understood by the
   *  interpolator. This simple ADT allows them to be presented either as string-like or
   *  XML-like types. */
  sealed trait Input
  case class StringLike(str: String) extends Input
  case class XmlLike(xml: XmlSeq) extends Input

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

  /** implementation of Contextual's contextualize method which determines the contexts of each
   *  of the holes in the interpolated string, and reports any parsing errors */
  def contextualize(interpolation: StaticInterpolation): Seq[ContextType] = {

    /** a representation of the current state used when parsing the XML interpolated string
     *
     *  @param part     the contextual [[StaticPart]] currently being parsed
     *  @param offset   the character offset within that part
     *  @param context  the parsing context of the current offset
     *  @param stack    the stack of tag names representing the path within the XML structure
     *  @param current  the value being parsed (which may be a tag name, attribute, etc)
     *  @param holes    the context types of the holes seen so far */
    case class ParseState(part: StaticPart,
                          offset: Int,
                          context: ContextType,
                          stack: List[String],
                          current: String,
                          holes: Vector[ContextType]) {
      
      /** starts a new part, and resets the offset counter */
      def apply(newPart: StaticPart) = copy(part = newPart, offset = 0)

      /** changes context, and increments the offset counter */
      def apply(newContext: ContextType) = copy(context = newContext, offset = offset + 1)
      
      /** changes context, increments the offset and appends [[char]] to the current value */
      def apply(newContext: ContextType, char: Char) =
        copy(context = newContext, current = current + char, offset = offset + 1)
      
      /** appends [[char]] to the current value and increments the offset */
      def apply(char: Char) = copy(offset = offset + 1, current = current + char)

      /** just increments the offset value */
      def apply() = copy(offset = offset + 1)

      /** aborts parsing, reporting the error message, [[msg]] */
      def abort(msg: String) = part match {
        case literal@Literal(_, _) =>
          interpolation.abort(literal, offset, msg)
        case hole@Hole(_, _) =>
          interpolation.abort(hole, msg)
      }

      /** resets the current value, without changing the offset */
      def reset() = copy(current = "")
      
      /** pushes a new tag name onto the stack */
      def push() = copy(stack = current :: stack, current = "")
      
      /** pops the tag name from the stack, and aborts if it does not match the current value,
       *  which should be the closing tag when this method is called */
      def pop() = stack.headOption match {
        case Some(`current`) => copy(stack = stack.tail)
        case Some(tag) => abort(s"closing tag '$current' does not match expected tag '$tag'")
        case None => rollback(current.length).abort(s"spurious closing tag: $current")
      }

      /** artificially change to an earlier offset; to be used to adjust the caret position
       *  before aborting */
      def rollback(difference: Int) = copy(offset = offset - difference)

      /** a string representation of the current state */
      override def toString() = s"part:${part.index}, off:$offset, "+
          s"stk:${stack.reverse.mkString("[", ".", "]")}, cur:'$current', ctx:$context"
    }

    /** a convenience case class for defining extractors which match on a set of valid
     *  characters */
    case class CharExtractor(chars: Set[Char]) {
      private[this] val charSet = chars.to[Set]
      def unapply(char: Char): Boolean = charSet.contains(char)
    }

    /** a set of letters */
    val Letters = ('a' to 'z').to[Set] ++ ('A' to 'Z').to[Set]

    /** a set of digits */
    val Digits = ('0' to '9').to[Set]

    /** extractor for characters which are valid in a tag name */
    val TagChar = CharExtractor(Letters ++ Digits + '.' + '-' + '_')
    
    /** extractor for whitespace characters */
    val WhitespaceChar = CharExtractor(Set(' ', '\t', '\n', '\r'))

    /** state machine parser for parsing the literal parts of an interpolated XML string
     *
     *  This operates as a simple fold which will be applied to the characters in each string
     *  in the context, for each character reading the current context and returning an updated
     *  state with a new context, along with potentially other changes.
     *
     *  The implementation makes heavy use of simplified methods defined on [[ParseState]]
     *  which construct an updated [[ParseState]] for each character. */
    def parseLiteral(state: ParseState, string: String): ParseState = string.foldLeft(state) {
      case (state, char) => state.context match {
        
        case TagName            => char match {
          case TagChar()          => state(char)
          case WhitespaceChar()   => state.push()(TagBody).reset()
          case ':'                => state(char) // FIXME: Namespaces
          case '/'                => if(state.current.isEmpty) state(ClosingTagName) else
                                         state(SelfClosingTag)
          case '>'                => state.push()(Body)
          case _                  => state.abort("not a valid tag name character")
        }
        
        case SelfClosingTag     => char match {
          case TagChar()          => state(char)
          case ':'                => state(char) // FIXME: Namespaces
          case '>'                => state(Body)
          case _                  => state.abort("expected '>'")
        }
        
        case ClosingTagName     => char match {
          case TagChar()          => state(char)
          case ':'                => state(char) // FIXME: Namespaces
          case '>'                => state.pop()(Body)
          case _                  => state.abort("expected '>'")
        }

        case AttributeName    => char match {
          case TagChar()          => state(char)
          case WhitespaceChar()   => state(AttributeName)
          case '>'                => state.abort("attribute value has not been specified")
          case '='                => state(AttributeEquals).reset()
          case ':'                => state(char) // FIXME: Namespaces
          case _                  => state.abort("not a valid attribute name character")
        }
        
        case AttributeEquals    => char match {
          case WhitespaceChar()   => state()
          case '"'                => state(AttributeValue)
          case _                  => state.abort("expected opening quote")
        }
        
        case AttributeValue     => char match {
          case '"'                => state(TagBody).reset()
          case '&'                => state(AttributeEntity)
          case char               => state(char)
        }
        
        case TagBody            => char match {
          case WhitespaceChar()   => state(TagBody)
          case TagChar()          => state(AttributeName, char)
          case '>'                => state(Body)
          case '/'                => state(TagClose)
          case _                  => state.abort("character not permitted in a tag name")
        }

        case TagClose           => char match {
          case '>'                => state(Body)
          case _                  => state.abort("expected '>'")
        }
        
        case Body               => char match {
          case '<'                => state(TagName).reset()
          case '&'                => state(BodyEntity).reset()
          case _                  => state()
        }
        
        case BodyEntity       => char match {
          case ';'                => state()
          case TagChar()          => state()
          case _                  => state.abort("not a valid entity name character")
        }

        case AttributeEntity  => char match {
          case ';'                => state()
          case TagChar()          => state()
          case _                  => state.abort("not a valid entity name character")
        }
      }
    }

    /** the initial parse state, before we have parsed anything */
    val initialParseState = ParseState(interpolation.parts.head, 0, Body, List(), "", Vector())
    
    /** the result of parsing all the literal parts of the interpolated string, and mapping
     *  over the holes
     *
     *  This folds over each part, parsing the literals using [[parseLiteral]], and mapping
     *  across the holes using the types of the substitutions. */
    val finalParseState = interpolation.parts.foldLeft(initialParseState) {
      
      case (state, Literal(_, literal)) =>
        parseLiteral(state, literal)
      
      case (state, hole@Hole(_, inputs)) =>
        inputs.get(state.context) match {
          case None =>
            state.abort("cannot substitute values of this type in this position")
          case Some(newContext) =>
            state.copy(context = newContext, holes = state.holes :+ state.context)
        }
    }

    // If we haven't closed all the tags we have opened, we abort.
    if(finalParseState.stack.nonEmpty)
      finalParseState.copy(offset = finalParseState.offset - 1).abort(
          s"expected closing tag: ${finalParseState.stack.head}")

    finalParseState.holes
  }

  /** evaluates the string using the actual substituted values by producing a single string and
   *  parsing it with an XML parser
   *
   *  @param interpolation  the runtime interpolation, provided by Contextual
   *  @return the [[XmlSeq]] resulting from parsing the contents of the interpolated string */
  def evaluate(interpolation: RuntimeInterpolation): XmlSeq =
    XmlSeq.parse(interpolation.parts.foldLeft("") {
      case (acc, Literal(_, literal)) => acc+literal
      case (acc, Substitution(_, StringLike(string))) => acc+string
      case (acc, Substitution(_, XmlLike(xml))) => acc+xml
    })

}

/** provides lower-priority implicits */
trait XmlInterpolator_1 { this: XmlInterpolator.type =>

  /** serializes anything that is convertible to `XmlSeq` */
  implicit def embedXmlSeqConvertibles[T: XmlSeq.Serializer] = XmlInterpolator.embed[T](
    Case(Body, Body) { x => XmlLike(implicitly[XmlSeq.Serializer[T]].serialize(x)) }
  )
}
