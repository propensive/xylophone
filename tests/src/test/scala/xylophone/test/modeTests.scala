package xylophone.test

import rapture.test.{Programme, TestSuite}
import xylophone._, backends.stdlib._

class XmlParsingModeTestsRun extends Programme {
  include(XmlParsingTests)
}

class XmlParsingModeTests(parser: Parser[String])  extends TestSuite {

  implicit val implicitParser: Parser[String] = parser

  val `Should return None in OptionMode for invalid xml` = test {
    import rapture.core.modes.returnOption._
    Xml.parse("<abc>10")
  } returns None

  val `Should return Some in OptionMode for valid xml` = test {
    import rapture.core.modes.returnOption._
    Xml.parse("<abc>10</abc>").map(_.toString())
  } returns Some("<abc>10</abc>")

}
