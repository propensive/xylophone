package xylophone.test

import rapture.test.{Programme, TestSuite}
import xylophone._

object ParsingTests extends TestSuite {

  val xmlSample =
    """
      |<a>
      |   <b>10</b>
      |   <b>11</b>
      |   <c>
      |     <d>18</d>
      |     <d>17</d>
      |     <d>16</d>
      |     <e>
      |       <w>
      |         <k>
      |             <o>hahaha</o>
      |         </k>
      |          <k>
      |             <o>seven days</o>
      |         </k>
      |       </w>
      |     </e>
      |   </c>
      |</a>
    """.stripMargin

  val `Xml with complex formatting should be parsed` = test {
    val sourceXml =
      "<a>\n \t  <b>10</b>\n   <b>11</b>\n   <c>\n  \t    <k>3</k>\n      <l>\n         <w><p>133</p><p>555</p></w>\n  <r>3123</r>     </l>\n   </c>\n   <d/>\n</a>"
    XmlSeq.parse(sourceXml)
  } returns XmlSeq.parse("<a><b>10</b><b>11</b><c><k>3</k><l><w><p>133</p><p>555</p></w><r>3123</r></l></c><d/></a>")

  val `Xml parser should trim text nodes` = test {
    XmlSeq.parse("<pod>Three <peas></peas> in the </pod>").toString()
  } returns "<pod>Three<peas/>in the</pod>"

  val `Parsed Xml should be equal to internal structure` = test {
    XmlSeq.parse("<a><b>10</b><b>11</b></a>").toString()
  } returns "<a><b>10</b><b>11</b></a>"

  val `Xml parser should parse plain string` = test {
    XmlSeq.parse("just some text").toString()
  } returns "just some text"

  val `Xml parser should parse empty tags` = test {
    XmlSeq.parse("<a></a>").toString()
  } returns "<a/>"


  //  //TODO: Need to add self closing mode?? or track it some how
  //  val `Xml parser should parse empty self enclosed tag` = test {
  //    XmlSeq.parse("<a/>").toString()
  //  } returns "<a/>" //Found <a></a> but expected <a/>
  //

  val `Xml parser should parse strings with self closed tag` = test {
    xml"just some text <ab></ab> yes".toString()
  } returns "just some text<ab/>yes"

  val `Xml API should be able to extract data by tag name` = test {
    XmlSeq.parse(xmlSample).a.*.c.*.e.*.w.*.toString()
  } returns "<k><o>hahaha</o></k><k><o>seven days</o></k>"

  val `Xml API should be able to extract data from array/sequence by index` = test {
    XmlSeq.parse(xmlSample).a().c().e().w().k().o().*.toString()
  } returns "hahaha"


  val `Getting not existing tag should return an empty XML` = test {
    xml"<a>hello</a>".abc.toString()
  } returns ""


  /*val `Parsing of invalid xml should lead to failure` = test {
    xml"<a> wwww <b>"
  } throws ParseException("<a> wwww <b>")*/

  val `Parsing XML with attributes ` = test {
    xml"""<abc a="11"><dd k="123" l="77"><a w="22">hello</a><a w="1">open</a></dd></abc>""".toString()
  } returns """<abc a="11"><dd l="77" k="123"><a w="22">hello</a><a w="1">open</a></dd></abc>"""

  val `Parsing XML with attributes and namespaces` = test {
    xml"""<z:Attachment rdf:about="#item_1" rdf:id="10">xxxxx</z:Attachment>""".toString()
  } returns """<z:Attachment rdf:id="10" rdf:about="#item_1">xxxxx</z:Attachment>"""


  ////TODO: Need to support xmlns
  //  val `Parsing XML with attributes and namespaces and xmlns` = test {
  //    Xml.parse("""<z:Attachment xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" rdf:about="#item_1"></z:Attachment>""").toString
  //  } returns """<z:Attachment xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" rdf:about="#item_1"></z:Attachment>"""


  val `Parsing XML with encoding attribute` = test {
    XmlSeq.parse("""<?xml version="1.0" encoding="UTF-8"?><a>wwww</a>""").toString()
  } returns "<a>wwww</a>"

  val `Parsing XML with encoding attribute and some formatting` = test {
    XmlSeq.parse(
      """<?xml
        |version="1.0"
        |encoding="UTF-8"
        |?><a>wwww</a>""".stripMargin).toString()
  } returns "<a>wwww</a>"

  val `Parsing XML with encoding attribute and only string/text` = test {
    XmlSeq.parse(
      """<?xml
        |version="1.0"
        |encoding="UTF-8"
        |?>
        |wwww""".stripMargin).toString()
  } returns "wwww"

  val `Get the node by index` = test {
    xml"<a><b>1</b></a><a><x>12</x></a>".a(1).toString()
  } returns "<a><x>12</x></a>"

  //TODO Throw exception for this case.
  //  val `Get the node by index that doesn't exist` = test {
  //    Xml.parse("<a><b>1</b></a>").a(10).toString()
  //  } returns ""

  val `Check XmlSeq index` = test {
    val x = XmlSeq.parse("<a><b>1</b></a>")
    x.a(0) == x(0)
  } returns false

  val `Get rest (*) of xml with inner nodes` = test {
    xml"<a><b>1</b></a><a><x>12</x></a>".a(1).*.toString()
  } returns "<x>12</x>"

  val `Get rest (*) of xml with text` = test {
    xml"<a><b>1</b></a><a>12</a>".a(1).*.toString()
  } returns "12"

}



