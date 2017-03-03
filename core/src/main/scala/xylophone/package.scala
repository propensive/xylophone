import contextual._

package object xylophone {
  
  implicit val implicitXmlStringParser: StdLibXmlStringParser.type = StdLibXmlStringParser

  /** implicit class providing the `xml` prefix for interpolated strings */
  implicit class XmlStringContext(stringContext: StringContext) {

    /** the `xml` prefix for interpolated strings */
    val xml = Prefix(XmlInterpolator, stringContext)
  }
}
