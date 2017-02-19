package xylophone.backends.stdlib

import xylophone._
import contextual._

object XmlInterpolator extends Interpolator {

  sealed trait ContextType extends Context
  case object XmlContent extends ContextType
  case object Attribute extends ContextType
  case object InAttribute extends ContextType

  def contextualize(interpolation: StaticInterpolation): Seq[ContextType] = Nil

  def evaluate(interpolation: RuntimeInterpolation): XmlSeq =
    Xml.parse(interpolation.literals.mkString)

}
