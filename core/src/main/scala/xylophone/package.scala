import contextual._

package object xylophone {
  implicit val implicitXmlStringParser: StdLibXmlStringParser.type = StdLibXmlStringParser

  implicit class XmlStringContext(stringContext: StringContext) {
    val xml = Prefix(XmlInterpolator, stringContext)
  }
}
