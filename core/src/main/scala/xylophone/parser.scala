package xylophone

import scala.annotation.implicitNotFound
import scala.util.Try

/** a simple SAM type class interface representing an XML parser for source types, such as
 *  [[String]] or [[java.nio.ByteBuffer]] */
@implicitNotFound(msg = "Cannot find an XML parser for values of type ${Source}")
trait Parser[-Source] { def parse(s: Source): Try[XmlSeq] }
