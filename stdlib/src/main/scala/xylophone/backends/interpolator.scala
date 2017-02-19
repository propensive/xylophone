package xylophone.backends.stdlib

import xylophone._
import contextual._

object XmlInterpolator extends Interpolator {

  sealed trait ContextType extends Context
  case object AttributeValue extends ContextType
  case object InTagName extends ContextType
  case object EndingTag extends ContextType
  case object InAttributeName extends ContextType
  case object InTagBody extends ContextType
  case object InClosingTag extends ContextType
  case object AttributeEquals extends ContextType
  case object Body extends ContextType
  case object InBodyEntity extends ContextType
  case object InAttributeEntity extends ContextType

  def contextualize(interpolation: StaticInterpolation): Seq[ContextType] = {

    case class ParseState(part: StaticPart,
                          offset: Int,
                          context: ContextType,
                          stack: List[String],
                          current: String,
                          holes: Vector[ContextType]) {
      
      def apply(newPart: StaticPart) = copy(part = newPart, offset = 0)
      def apply(newContext: ContextType) = copy(context = newContext, offset = offset + 1)
      
      def apply(newContext: ContextType, char: Char) =
        copy(context = newContext, current = current + char, offset = offset + 1)
      
      def apply(char: Char) = copy(offset = offset + 1, current = current + char)
      def apply() = copy(offset = offset + 1)

      def abort(msg: String) = {
        println(this)
        part match {
          case literal@Literal(_, _) =>
            interpolation.abort(literal, offset, msg)
          case hole@Hole(_, _) =>
            interpolation.abort(hole, msg)
        }
      }

      def push(tag: String) = copy(offset = offset + 1, stack = tag :: stack)
      
      def reset() = copy(current = "")
      def pop(tag: String) = stack.headOption match {
        case Some(`tag`) => stack.tail
        case Some(_)     => abort("closing tag does not match")
        case None        => abort("spurious closing tag")
      }

      override def toString() = s"part:${part.index}, off:$offset, stk:${stack.reverse.mkString(".")}, cur:'$current', ctx:$context"
    }

    val initialParseState = ParseState(interpolation.parts.head, 0, Body, List(), "", Vector())

    case class CharExtractor(chars: Set[Char]) {
      val charSet = chars.to[Set]
      def unapply(char: Char): Boolean = charSet.contains(char)
    }

    val Letters = ('a' to 'z').to[Set] ++ ('A' to 'Z').to[Set]
    val Digits = ('0' to '9').to[Set]

    //val InitialTagChar = CharExtractor(Letters + '_')
    val TagChar = CharExtractor(Letters ++ Digits + '.' + '-' + '_')
    val WhitespaceChar = CharExtractor(Set(' ', '\t', '\n', '\r'))

    def parseLiteral(state: ParseState, string: String): ParseState = string.foldLeft(state) {
      case (state@ParseState(_, _, _, _, _, _), char) => state.context match {
        
        case InTagName         => char match {
          case TagChar()         => state(char)
          case WhitespaceChar()  => state(InTagBody).reset()
          case ':'               => state(char) // FIXME: Namespaces
          case '/'               => state(if(state.current.isEmpty) EndingTag else InTagName)
          case '>'               => state(Body)
          case _                 => state.abort("FIXME")
        }
        
        case EndingTag         => char match {
          case TagChar()         => state(char)
          case ':'               => state(char)
          case '>'               => state(Body)
          case _                 => state.abort("expected '>'")
        }

        case InAttributeName   => char match {
          case TagChar()         => state(char)
          case WhitespaceChar()  => state(InAttributeName)
          case '='               => state(AttributeEquals)
          case ':'               => state(char) // FIXME: Namespaces
          case _                 => state.abort("FIXME")
        }
        
        case AttributeEquals   => char match {
          case WhitespaceChar()  => state()
          case '"'               => state(AttributeValue)
          case _                 => state.abort("FIXME")
        }
        
        case AttributeValue    => char match {
          case '"'               => state(InTagBody)
          case '&'               => state(InAttributeEntity)
          case char              => state(char)
        }
        
        case InTagBody         => char match {
          case WhitespaceChar()  => state(InTagBody)
          case TagChar()         => state(InAttributeName, char)
          case '>'               => state(Body)
          case _                 => state.abort("character not permitted in a tag name")
        }
        
        case InClosingTag      => char match {
          case TagChar()         => state(char)
          case WhitespaceChar()  => state()
          case _                 => state.abort("")
        }
        
        case Body              => char match {
          case '<'               => state(InTagName).reset()
          case '&'               => state(InBodyEntity).reset()
          case _                 => state()
        }
        
        case InBodyEntity      => char match {
          case ';'               => state()
          case _                 => state.abort("not a valid character")
        }

        case InAttributeEntity => char match {
          case ';'               => state()
          case TagChar()         => state()
          case _                 => state.abort("FIXME")
        }
      }
    }

    val finalParseState = interpolation.parts.foldLeft(initialParseState) {
      
      case (state, Literal(_, literal)) =>
        parseLiteral(state, literal)
      
      case (state, Hole(_, inputs)) =>
        state.copy(context = inputs(state.context), holes = state.holes :+ state.context)

    }

    if(finalParseState.stack.nonEmpty) finalParseState.abort("missing closing tag(s)")

    finalParseState.holes
  }

  def evaluate(interpolation: RuntimeInterpolation): XmlSeq =
    Xml.parse(interpolation.literals.mkString)

}
