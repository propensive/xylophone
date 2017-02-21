package xylophone.backends

import contextual._

package object stdlib {
  implicit val implicitXmlStringParser: StdLibXmlStringParser.type = StdLibXmlStringParser

  implicit class XmlStringContext(stringContext: StringContext) {
    val xml = Prefix(XmlInterpolator, stringContext)
  }
}
