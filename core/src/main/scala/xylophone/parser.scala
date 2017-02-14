package xylophone

import scala.annotation.implicitNotFound
import scala.util.Try

@implicitNotFound(msg = "Cannot find an XML parser for values of type ${Source}")
trait Parser[-Source] {
  def parse(s: Source): Try[XmlSeq]
}
