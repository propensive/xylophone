package xylophone.test

import rapture.test.TestSuite
import xylophone._

object SerializationTests extends TestSuite {
  
  case class Address(id: Int, users: List[User])
  case class User(name: String)

  val `Should serialize custom case classes` = test {
    XmlSeq(Address(1, List(User("Alice"), User("Dave")))).toString()
  } returns "<id>1</id><users><user><name>Alice</name></user><user><name>Dave</name></user></users>"

  val `Should serialize list of custom case classes` = test {
    XmlSeq(List(Address(1, List(User("Alice"), User("Dave"))))).toString()
  } returns "<address><id>1</id><users><user><name>Alice</name></user><user><name>Dave</name></user></users></address>"

  val `Should serialize list of custom case classes with custom seq type tag` = test {
    implicit val myTag = SeqTag[List[Address]]("test")
    XmlSeq(List(Address(1, List(User("Alice"), User("Dave"))))).toString()
  } returns "<test><id>1</id><users><user><name>Alice</name></user><user><name>Dave</name></user></users></test>"

  val `Should serialize list with default type` = test {
    XmlSeq(List(1,2,3,4)).toString()
  } returns "<int>1</int><int>2</int><int>3</int><int>4</int>"

  val `Should serialize list with custom seq type tag` = test {
    implicit val myTag = SeqTag[List[Int]]("test")
    XmlSeq(List(1,2,3,4)).toString()
  } returns "<test>1</test><test>2</test><test>3</test><test>4</test>"


  val `Serialize case class by XmlNode` = test {
    implicit val userWrapTag: WrapTag[User] = WrapTag("user")
    XmlNode(User("Igor")).toString()
  } returns "<user><name>Igor</name></user>"

  val `Serialize complex case class by XmlNode` = test {
    implicit val userWrapTag: WrapTag[Address] = WrapTag("address")
    XmlNode(Address(1, List(User("Alice"), User("Dave")))).toString()
  } returns "<address><id>1</id><users><user><name>Alice</name></user><user><name>Dave</name></user></users></address>"

  val `Serialize list of case classes by XmlNode` = test {
    implicit val userSeqTag: SeqTag[List[User]] = SeqTag("user")
    implicit val userWrapTag: WrapTag[List[User]] = WrapTag("users")
    XmlNode(List(User("Alice"), User("Dave"))).toString()
  } returns "<users><user><name>Alice</name></user><user><name>Dave</name></user></users>"

  val `Test implicit Boolean serializer` = test(XmlSeq(false).toString()).returns("false")
  val `Test implicit Byte serializer` = test(XmlSeq(1.toByte).toString()).returns("1")
  val `Test implicit Short serializer` = test(XmlSeq(1).toString()).returns("1")
  val `Test implicit Int serializer` = test(XmlSeq(1).toString()).returns("1")
  val `Test implicit Long serializer` = test(XmlSeq(1L).toString()).returns("1")
  val `Test implicit Double serializer` = test(XmlSeq(1.0).toString()).returns("1.0")
  val `Test implicit Float serializer` = test(XmlSeq(1.toFloat).toString()).returns("1.0")
  val `Test implicit String serializer` = test(XmlSeq("hello").toString()).returns("hello")
  val `Test implicit BigDecimal serializer` = test(XmlSeq(BigDecimal(1212)).toString()).returns("1212")
  val `Test implicit BigInt serializer` = test(XmlSeq(BigInt(2323)).toString()).returns("2323")
  val `Test implicit Some(...) serializer` = test(XmlSeq[Option[String]](Some("abc")).toString()).returns("abc")
  val `Test implicit None serializer` = test(XmlSeq(None).toString()).returns("")

}

