package xylophone

import xylophone.Ast.{DefaultNamespace, Element, Name, Namespace, Text}

import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.language.higherKinds
import scala.reflect.ClassTag


@implicitNotFound("Cannot serialize type ${T} to XmlSeq Please provide an implicit Serializer of type ${T} to XmlSeq")
trait SeqSerializer[T] {
  self =>
  def serialize(t: T): XmlSeq

  def contramap[T2](fn: T2 => T) = new SeqSerializer[T2] {
    def serialize(t: T2): XmlSeq = self.serialize(fn(t))
  }
}

@implicitNotFound("Cannot serialize type ${T} to XmlSeq Please provide an implicit Serializer of type ${T} to XmlSeq")
trait NodeSerializer[T] {
  self =>
  def serialize(t: T): XmlNode

  def contramap[T2](fn: T2 => T) = new NodeSerializer[T2] {
    def serialize(t: T2): XmlNode = self.serialize(fn(t))
  }
}

object SeqSerializer {
  def fromMap(map: ListMap[String, XmlSeq])(implicit namespace: Namespace): XmlSeq = {
    map
      .to[List]
      .foldLeft(XmlSeq.Empty){
        case (xml, (k, v: XmlSeq)) =>
          XmlSeq(xml.elements :+ Element(Name(namespace, k), Map(), v.elements))
      }
  }
}

@implicitNotFound("Cannot find WrapTag typeclass")
case class WrapTag[T](name: String)

@implicitNotFound("Cannot find SeqTag typeclass")
case class SeqTag[T](name: String)

private[xylophone] case class BasicXmlSerializer[T](serialization: T => XmlSeq) extends SeqSerializer[T] {
  def serialize(t: T): XmlSeq = serialization(t)
}

trait XmlSeqSerializers {

  private[xylophone] def defaultSerializer[T] = BasicXmlSerializer[T](x => XmlSeq(Text(x.toString)))
  implicit def defaultSeqTag[T: ClassTag] = SeqTag[T](implicitly[ClassTag[T]].runtimeClass.getSimpleName.toLowerCase())

  implicit def byteSerializer: SeqSerializer[Byte] = defaultSerializer
  implicit def shortSerializer: SeqSerializer[Short] = defaultSerializer
  implicit def intSerializer: SeqSerializer[Int] = defaultSerializer
  implicit def longSerializer: SeqSerializer[Long] = defaultSerializer
  implicit def booleanSerializer: SeqSerializer[Boolean] = defaultSerializer
  implicit def stringSerializer: SeqSerializer[String] = defaultSerializer
  implicit def floatSerializer: SeqSerializer[Float] = defaultSerializer
  implicit def doubleSerializer: SeqSerializer[Double] = defaultSerializer
  implicit def bigDecimalSerializer: SeqSerializer[BigDecimal] = defaultSerializer
  implicit def bigIntSerializer: SeqSerializer[BigInt] = defaultSerializer
  implicit def nilSerializer: SeqSerializer[Nil.type] = BasicXmlSerializer(x => XmlSeq(x))

  implicit def optionSerializer[T](implicit ser: SeqSerializer[T]): SeqSerializer[Option[T]] =
    BasicXmlSerializer(_.map(ser.serialize).getOrElse(XmlSeq(Text(""))))

  implicit def traversableSerializer[Type, S[T] <: Traversable[T]](implicit ser: SeqSerializer[Type], tag: SeqTag[Type]): SeqSerializer[S[Type]] = {
    BasicXmlSerializer { seq =>
      seq.foldLeft[XmlSeq](XmlSeq.Empty)((xml, el) =>
        XmlSeq(
          xml.elements :+ Element(Name(DefaultNamespace, tag.name), Map(), ser.serialize(el).elements))
      )
    }
  }

}

trait XmlNodeSerializers {

  implicit def seqToNode[T](implicit ser: SeqSerializer[T], wrapTag: WrapTag[T]): NodeSerializer[T] = {
    new NodeSerializer[T] {
      override def serialize(t: T): XmlNode = XmlNode(Seq(Element(Name(DefaultNamespace,wrapTag.name), Map(), ser.serialize(t).elements)), Vector())
    }
  }

}