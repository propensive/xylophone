package xylophone.test

import rapture.test.{Programme, TestSuite}
import xylophone._
import xylophone.XmlSeq._

import scala.collection.immutable.ListMap

class XmlSerializationTestsRun extends Programme {
  include(XmlSerializationTests)
}

object XmlSerializationTests extends TestSuite {

  implicit val implicitParser: Parser[String] = backends.stdlib.implicitXmlStringParser

  case class Address(id: Int, users: List[User])
  case class User(name: String)

  implicit val userSerializer: XmlSeq.SeqSerializer[User] = (user: User) => {
    SeqSerializer.fromMap(ListMap("name" -> stringSerializer.serialize(user.name)))
  }

  implicit val serializer: XmlSeq.SeqSerializer[Address] = (address: Address) => {
    SeqSerializer.fromMap(ListMap(
      "id" ->  implicitly[XmlSeq.SeqSerializer[Int]].serialize(address.id),
      "users" -> XmlSeq(implicitly[XmlSeq.SeqSerializer[List[User]]].serialize(address.users).$root)
    ))
  }

  val `Should serialize custom case classes` = test {
    Xml(Address(1, List(User("Alice"), User("Dave")))).toString()
  } returns "<id>1</id><users><user><name>Alice</name></user><user><name>Dave</name></user></users>"

  val `Should serialize list of custom case classes` = test {
    Xml(List(Address(1, List(User("Alice"), User("Dave"))))).toString()
  } returns "<address><id>1</id><users><user><name>Alice</name></user><user><name>Dave</name></user></users></address>"

  val `Should serialize list of custom case classes with custom seq type tag` = test {
    implicit val myTag = SeqTag[List[Address]]("test")
    Xml(List(Address(1, List(User("Alice"), User("Dave"))))).toString()
  } returns "<test><id>1</id><users><user><name>Alice</name></user><user><name>Dave</name></user></users></test>"

  val `Should serialize list with default type` = test {
    Xml(List(1,2,3,4)).toString()
  } returns "<int>1</int><int>2</int><int>3</int><int>4</int>"

  val `Should serialize list with custom seq type tag` = test {
    implicit val myTag = SeqTag[List[Int]]("test")
    Xml(List(1,2,3,4)).toString()
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

  val `Test implicit Boolean serializer` = test(Xml[Boolean](false).toString()).returns("false")
  val `Test implicit Byte serializer` = test(Xml[Byte](1).toString()).returns("1")
  val `Test implicit Short serializer` = test(Xml[Short](1).toString()).returns("1")
  val `Test implicit Int serializer` = test(Xml[Int](1).toString()).returns("1")
  val `Test implicit Long serializer` = test(Xml[Long](1L).toString()).returns("1")
  val `Test implicit Double serializer` = test(Xml[Double](1.0).toString()).returns("1.0")
  val `Test implicit Float serializer` = test(Xml[Float](1).toString()).returns("1.0")
  val `Test implicit String serializer` = test(Xml[String]("hello").toString()).returns("hello")
  val `Test implicit BigDecimal serializer` = test(Xml[BigDecimal](BigDecimal(1212)).toString()).returns("1212")
  val `Test implicit BigInt serializer` = test(Xml[BigInt](BigInt(2323)).toString()).returns("2323")
  val `Test implicit Nil serializer` = test(Xml[Nil.type](Nil).toString()).returns("")
  val `Test implicit Some(...) serializer` = test(Xml[Option[String]](Some("abc")).toString()).returns("abc")
  val `Test implicit None serializer` = test(Xml[Option[String]](None).toString()).returns("")

}

