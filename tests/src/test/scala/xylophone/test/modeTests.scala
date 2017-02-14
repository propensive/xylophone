package xylophone.test

import rapture.test.{Programme, TestSuite}
import xylophone.{Parser, Xml}

class XmlParsingModeTestsRun extends Programme {
  include(new XmlParsingTests(xylophone.backends.stdlib.implicitXmlStringParser))
}

class XmlParsingModeTests(parser: Parser[String, Xml])  extends TestSuite {

  implicit val implicitParser: Parser[String, Xml] = parser

  val `Should return None in OptionMode for invalid xml` = test {
    import rapture.core.modes.returnOption._
    Xml.parse("<abc>10")
  } returns None

  val `Should return Some in OptionMode for valid xml` = test {
    import rapture.core.modes.returnOption._
    Xml.parse("<abc>10</abc>").map(_.toString())
  } returns Some("<abc>10</abc>")

}
