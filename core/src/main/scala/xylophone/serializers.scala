package xylophone

import xylophone.Ast._

import scala.annotation.implicitNotFound
import scala.collection.immutable.ListMap
import scala.language.higherKinds
import scala.reflect.ClassTag
import language.experimental.macros
/** typeclass interface for defining the name of the tag to be used to surround sequence
 *  elements */
@implicitNotFound("Cannot find WrapTag typeclass for ${T}")
case class WrapTag[T](name: String)

/** typeclass interface for defining the name of the tag to be used to surround sequences */
@implicitNotFound("Cannot find SeqTag typeclass for ${T}")
case class SeqTag[+T](name: String)

/** companion object to SeqTag, providing a default implicit */
object SeqTag {

  /** implicit [[SeqTag]] instance which generates a surrounding tag name from the class name
   */
  implicit def defaultSeqTag[T: ClassTag, F[t] <: Traversable[t]]: SeqTag[F[T]] = SeqTag("item")
}

/** SAM typeclass interface for serialization to [[XmlSeq]]s */
@implicitNotFound("Cannot serialize type ${T} to XmlSeq. Please provide an implicit "+
"SeqSerializer of type ${T} to XmlSeq")
trait SeqSerializer[-T] { serializer =>

  /** abstract method to specify the conversion to [[XmlSeq]] */
  def serialize(t: T): XmlSeq

  /** constructs a new [[SeqSerializer]] from this, which pre-applies the [[fn]] function to
    *  the value prior to serialization */
  def contramap[T2](fn: T2 => T): SeqSerializer[T2] = t => serializer.serialize(fn(t))

  def orElse[TS <: T, T2 >: TS](fallbackSerializer: => SeqSerializer[T2]): SeqSerializer[TS] = new SeqSerializer[TS] {
    def serialize(t: TS): XmlSeq = {
      try {
        serializer.serialize(t)
      } catch {
        case e: Exception =>
          fallbackSerializer.serialize(t)
      }
    }
  }
}

@implicitNotFound("Cannot serialize type ${T} to XmlSeq. Please provide an implicit "+
  "SeqSerializer of type ${T} to XmlSeq")
trait SeqSerializerWrapper[T] {
  def serializer: SeqSerializer[T]
}

/** companion object to the [[SeqSerializer]] typeclass interface */
object SeqSerializer {

  def fromMap(map: ListMap[String, XmlSeq])(implicit namespace: Namespace): XmlSeq =
    map.foldLeft(XmlSeq.Empty) {
      case (xml, (k, v: XmlSeq)) =>
        XmlSeq(xml.$root :+ Element(Name(namespace, k), Map(), v.$root))
    }
}

/** mixin providing serialization for [[XmlSeq]]s */
trait XmlSeqSerializers extends XmlSeqSerializers_1 {

  /** serializes bytes to XML text */
  implicit def byteSerializer: SeqSerializer[Byte] = x => XmlSeq(Text(x.toString))

  /** serializes shorts to XML text */
  implicit def shortSerializer: SeqSerializer[Short] = x => XmlSeq(Text(x.toString))

  /** serializes ints to XML text */
  implicit def intSerializer: SeqSerializer[Int] = x => XmlSeq(Text(x.toString))

  /** serializes longs to XML text */
  implicit def longSerializer: SeqSerializer[Long] = x => XmlSeq(Text(x.toString))

  /** serializes booleans to XML text, as the values "true" and "false" */
  implicit def booleanSerializer: SeqSerializer[Boolean] = x => XmlSeq(Text(x.toString))

  /** serializes strings to XML text */
  implicit def stringSerializer: SeqSerializer[String] = x => XmlSeq(Text(x))

  /** serializes floats to XML text */
  implicit def floatSerializer: SeqSerializer[Float] = x => XmlSeq(Text(x.toString))

  /** serializes doubles to XML text */
  implicit def doubleSerializer: SeqSerializer[Double] = x => XmlSeq(Text(x.toString))

  /** serializes [[BigDecimal]]s to XML text */
  implicit def bigDecimalSerializer: SeqSerializer[BigDecimal] = x => XmlSeq(Text(x.toString))

  /** serializes [[BigInt]]s to XML text */
  implicit def bigIntSerializer: SeqSerializer[BigInt] = x => XmlSeq(Text(x.toString))

}

/** lower-priority serializer implicits */
trait XmlSeqSerializers_1 { this: XmlSeqSerializers =>

  /** serializes [[Option]]s to their value in the [[Some]] case, or an empty sequence
   *  otherwise */
  implicit def optionSerializer[T](implicit ser: SeqSerializer[T]): SeqSerializer[Option[T]] =
    _.map(ser.serialize).getOrElse(XmlSeq(Seq(), Vector()))

  /** serializes any traversible from the Scala collections library to an XML sequence,
   *  provided the element type can be serialized */
  implicit def traversableSerializer[Type](implicit serializer: SeqSerializer[Type],
                                           tag: SeqTag[Traversable[Type]],
                                           namespace: Namespace): SeqSerializer[Traversable[Type]] =
    _.foldLeft[XmlSeq](XmlSeq.Empty) { (xml, el) =>
      val elements = serializer.serialize(el).$root
      val node = Element(Name(namespace, tag.name), Map(), elements)
      XmlSeq(xml.$root :+ node)
    }

  /** serializes arrays from the Scala collections library to an XML sequence,
    *  provided the element type can be serialized */
  implicit def arraySerializer[T: ClassTag](implicit serializer: SeqSerializer[T],
                                             tag: SeqTag[Array[T]],
                                             namespace: Namespace): SeqSerializer[Array[T]] = {
    _.foldLeft[XmlSeq](XmlSeq.Empty) { (xml, el) =>
      val elements = serializer.serialize(el).$root
      val node = Element(Name(namespace, tag.name), Map(), elements)
      XmlSeq(xml.$root :+ node)
    }
  }



  implicit def xmlSerializerMacro2[T]: SeqSerializerWrapper[T] = macro XmlMacros.serializerWrapperMacro[T]

  implicit def xmlSerializerMacro[T](implicit wrapper: SeqSerializerWrapper[T]): SeqSerializer[T] = wrapper.serializer
}

/** SAM typeclass interface for serialization to [[XmlNode]]s */
@implicitNotFound("Cannot serialize type ${T} to XmlNode. Please provide an implicit "+
  "Serializer of type ${T} to XmlNode.")
trait NodeSerializer[-T] { nodeSerializer =>

  /** abstract method to specify the conversion to [[XmlNode]] */
  def serialize(t: T): XmlNode

  /** constructs a new [[NodeSerializer]] from this, which pre-applies the [[fn]] function to
    *  the value prior to serialization */
  def contramap[T2](fn: T2 => T): NodeSerializer[T2] =
    t => nodeSerializer.serialize(fn(t))
}

/** mixin providing serialization for [[XmlNode]]s */
trait XmlNodeSerializers {

  /** typeclass generator for providing a valid [[NodeSerializer]] if corresponding [[WrapTag]]
   *  and [[SeqSerializer]] typeclass instances are available */
  implicit def seqToNode[T](implicit serializer: SeqSerializer[T],
                                     wrapTag: WrapTag[T],
                                     namespace: Namespace): NodeSerializer[T] = { (t: T) =>

    val children = serializer.serialize(t).$root
    XmlNode(Seq(Element(Name(namespace, wrapTag.name), Map(), children)), Vector())
  }
}
