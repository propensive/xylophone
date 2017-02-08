package xylophone

import scala.annotation.implicitNotFound
import scala.util.Try

@implicitNotFound(msg = "Cannot find ${Data} parser for values of type ${Source}")
trait Parser[-Source, +Data <: Xml] {
  def parse(s: Source): Try[Data]
}