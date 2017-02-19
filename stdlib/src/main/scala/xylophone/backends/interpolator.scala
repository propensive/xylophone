package xylophone.backends.stdlib

import xylophone._
import contextual._

object XmlInterpolator extends Interpolator {

  sealed trait ContextType extends Context
  case object AttributeValue extends ContextType
  case object InTagName extends ContextType
  case object SelfClosingTagName extends ContextType
  case object TagClose extends ContextType
  case object ClosingTag extends ContextType
  case object InAttributeName extends ContextType
  case object InTagBody extends ContextType
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

      def reset() = copy(current = "")
      
      def push() = copy(stack = current :: stack, current = "")
      
      def pop() = stack.headOption match {
        case Some(`current`) => copy(stack = stack.tail)
        case Some(tag) => abort(s"closing tag '$current' does not match expected tag '$tag'")
        case None => rollback(current.length).abort(s"spurious closing tag: $current")
      }

      def rollback(difference: Int) = copy(offset = offset - difference)

      override def toString() = s"part:${part.index}, off:$offset, "+
          s"stk:${stack.reverse.mkString("[", ".", "]")}, cur:'$current', ctx:$context"
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
        
        case InTagName          => char match {
          case TagChar()          => state(char)
          case WhitespaceChar()   => state.push()(InTagBody).reset()
          case ':'                => state(char) // FIXME: Namespaces
          case '/'                => if(state.current.isEmpty) state(ClosingTag) else
                                        state(SelfClosingTagName)
          case '>'                => state.push()(Body)
          case _                  => state.abort("not a valid tag name character")
        }
        
        case SelfClosingTagName => char match {
          case TagChar()          => state(char)
          case ':'                => state(char) // FIXME: Namespaces
          case '>'                => state(Body)
          case _                  => state.abort("expected '>'")
        }
        
        case ClosingTag         => char match {
          case TagChar()          => state(char)
          case ':'                => state(char) // FIXME: Namespaces
          case '>'                => state.pop()(Body)
          case _                  => state.abort("expected '>'")
        }

        case InAttributeName    => char match {
          case TagChar()          => state(char)
          case WhitespaceChar()   => state(InAttributeName)
          case '='                => state(AttributeEquals).reset()
          case ':'                => state(char) // FIXME: Namespaces
          case _                  => state.abort("not a valid attribute name character")
        }
        
        case AttributeEquals    => char match {
          case WhitespaceChar()   => state()
          case '"'                => state(AttributeValue)
          case _                  => state.abort("expected '='")
        }
        
        case AttributeValue     => char match {
          case '"'                => state(InTagBody).reset()
          case '&'                => state(InAttributeEntity)
          case char               => state(char)
        }
        
        case InTagBody          => char match {
          case WhitespaceChar()   => state(InTagBody)
          case TagChar()          => state(InAttributeName, char)
          case '>'                => state(Body)
          case '/'                => state(TagClose)
          case _                  => state.abort("character not permitted in a tag name")
        }

        case TagClose           => char match {
          case '>'                => state(Body)
          case _                  => state.abort("expected '>'")
        }
        
        
        case Body               => char match {
          case '<'                => state(InTagName).reset()
          case '&'                => state(InBodyEntity).reset()
          case _                  => state()
        }
        
        case InBodyEntity       => char match {
          case ';'                => state()
          case _                  => state.abort("not a valid entity name character")
        }

        case InAttributeEntity  => char match {
          case ';'                => state()
          case TagChar()          => state()
          case _                  => state.abort("not a valid entity name character")
        }
      }
    }

    val finalParseState = interpolation.parts.foldLeft(initialParseState) {
      
      case (state, Literal(_, literal)) =>
        parseLiteral(state, literal)
      
      case (state, Hole(_, inputs)) =>
        state.copy(context = inputs(state.context), holes = state.holes :+ state.context)

    }

    if(finalParseState.stack.nonEmpty)
      finalParseState.copy(offset = finalParseState.offset - 1).abort(
          s"expected closing tag: ${finalParseState.stack.head}")

    finalParseState.holes
  }

  def evaluate(interpolation: RuntimeInterpolation): XmlSeq =
    Xml.parse(interpolation.literals.mkString)

}
