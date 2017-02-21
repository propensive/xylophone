package xylophone

import xylophone.Ast.{Element, Name, Namespace, Text}

import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.language.higherKinds
import scala.reflect.ClassTag

import language.experimental.macros

@implicitNotFound("Cannot find WrapTag typeclass for ${T}")
case class WrapTag[T](name: String)

@implicitNotFound("Cannot find SeqTag typeclass for ${T}")
case class SeqTag[T](name: String)

trait SeqSerializer[T] { seqSerializer =>
  
  def serialize(t: T): XmlSeq

  def contramap[T2](fn: T2 => T): SeqSerializer[T2] = t => seqSerializer.serialize(fn(t))
}

object SeqSerializer extends XmlSeqSerializers {
  
  def fromMap(map: ListMap[String, XmlSeq])(implicit namespace: Namespace): XmlSeq =
    map.foldLeft(XmlSeq.Empty) {
      case (xml, (k, v: XmlSeq)) =>
        XmlSeq(xml.$root :+ Element(Name(namespace, k), Map(), v.$root))
    }
}

trait XmlSeqSerializers_2 {
  implicit def xmlSerializerMacro[T]: SeqSerializer[T] = macro XmlMacros.serializerMacro[T]
}

trait XmlSeqSerializers_1 extends XmlSeqSerializers_2 {
}

trait XmlSeqSerializers extends XmlSeqSerializers_1 {

  @implicitNotFound("Cannot serialize type ${T} to XmlSeq. Please provide an implicit "+
      "Serializer of type ${T} to XmlSeq")
  implicit def defaultSeqTag[T: ClassTag, F[_]]: SeqTag[F[T]] =
    SeqTag(implicitly[ClassTag[T]].runtimeClass.getSimpleName.toLowerCase())
  
  implicit val byteSerializer: SeqSerializer[Byte] = x => XmlSeq(Text(x.toString))
  implicit val shortSerializer: SeqSerializer[Short] = x => XmlSeq(Text(x.toString))
  implicit val intSerializer: SeqSerializer[Int] = x => XmlSeq(Text(x.toString))
  implicit val longSerializer: SeqSerializer[Long] = x => XmlSeq(Text(x.toString))
  implicit val booleanSerializer: SeqSerializer[Boolean] = x => XmlSeq(Text(x.toString))
  implicit val stringSerializer: SeqSerializer[String] = x => XmlSeq(Text(x))
  implicit val floatSerializer: SeqSerializer[Float] = x => XmlSeq(Text(x.toString))
  implicit val doubleSerializer: SeqSerializer[Double] = x => XmlSeq(Text(x.toString))
  implicit val bigDecimalSerializer: SeqSerializer[BigDecimal] = x => XmlSeq(Text(x.toString))
  implicit val bigIntSerializer: SeqSerializer[BigInt] = x => XmlSeq(Text(x.toString))
  implicit val nilSerializer: SeqSerializer[Nil.type] = XmlSeq(_)
  
  implicit def optionSerializer[T](implicit ser: SeqSerializer[T]): SeqSerializer[Option[T]] =
    _.map(ser.serialize).getOrElse(XmlSeq(Text("")))

  implicit def traversableSerializer[Type, S[T] <: Traversable[T]](
      implicit seqSerializer: SeqSerializer[Type],
               tag: SeqTag[S[Type]],
               namespace: Namespace): SeqSerializer[S[Type]] =
    _.foldLeft[XmlSeq](XmlSeq.Empty) { (xml, el) =>
      val elements = seqSerializer.serialize(el).$root
      val node = Element(Name(namespace, tag.name), Map(), elements)
      XmlSeq(xml.$root :+ node)
    }
}

trait XmlNodeSerializers {

  @implicitNotFound("Cannot serialize type ${T} to XmlNode. Please provide an implicit "+
      "Serializer of type ${T} to XmlNode")
  trait NodeSerializer[T] { nodeSerializer =>
    
    def serialize(t: T): XmlNode

    def contramap[T2](fn: T2 => T): NodeSerializer[T2] =
      t => nodeSerializer.serialize(fn(t))
  }

  implicit def seqToNode[T](implicit seqSerializer: SeqSerializer[T],
                                     wrapTag: WrapTag[T],
                                     namespace: Namespace): NodeSerializer[T] = { (t: T) =>

    val children = seqSerializer.serialize(t).$root
    XmlNode(Seq(Element(Name(namespace, wrapTag.name), Map(), children)), Vector())
  }

}
