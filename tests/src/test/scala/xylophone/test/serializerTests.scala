package xylophone.test

import rapture.test.{Programme, TestSuite}
import xylophone.traversableSerializer
import xylophone._

import scala.collection.immutable.ListMap

class XmlSerializationTestsRun extends Programme {
  include(new XmlSerializationTests(xylophone.backends.stdlib.implicitXmlStringParser))
}

class XmlSerializationTests(parser: Parser[String]) extends TestSuite {

  implicit val implicitParser: Parser[String] = parser

  case class Address(id: Int, users: List[User])
  case class User(name: String)

  implicit val userSerializer = new SeqSerializer[User] {
    override def serialize(t: User): XmlSeq = {
      SeqSerializer.fromMap(ListMap("name" -> stringSerializer.serialize(t.name)))
    }
  }

  implicit val serializer = new SeqSerializer[Address] {
    override def serialize(t: Address): XmlSeq = {
      SeqSerializer.fromMap(ListMap(
        "id" -> intSerializer.serialize(t.id),
        "users" -> XmlSeq(traversableSerializer.serialize(t.users).elements)
      ))
    }
  }

  val `Should serialize custom case classes` = test {
    Xml(Address(1, List(User("Alice"), User("Dave")))).toString()
  } returns "<id>1</id><users><user><name>Alice</name></user><user><name>Dave</name></user></users>"

  val `Should serialize list of custom case classes` = test {
    Xml(List(Address(1, List(User("Alice"), User("Dave"))))).toString()
  } returns "<address><id>1</id><users><user><name>Alice</name></user><user><name>Dave</name></user></users></address>"


  val `Should serialize list with default type` = test {
    Xml(List(1,2,3,4)).toString()
  } returns "<int>1</int><int>2</int><int>3</int><int>4</int>"

  val `Should serialize list with custom type tag` = test {
    implicit val myTag = SeqTag[Int]("test")
    Xml(List(1,2,3,4)).toString()
  } returns "<test>1</test><test>2</test><test>3</test><test>4</test>"




}

