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
case class SeqTag[T](name: String)

/** companion object to SeqTag, providing a default implicit */
object SeqTag {
  
  /** implicit [[SeqTag]] instance which generates a surrounding tag name from the class name
   */
  implicit def defaultSeqTag[T: ClassTag, F[_]]: SeqTag[F[T]] =
    SeqTag(implicitly[ClassTag[T]].runtimeClass.getSimpleName.toLowerCase())
}

/** mixin providing serialization for [[XmlSeq]]s */
trait XmlSeqSerializers extends XmlSeqSerializers_1 {

  /** SAM typeclass interface for serialization to [[XmlSeq]]s */
  @implicitNotFound("Cannot serialize type ${T} to XmlSeq. Please provide an implicit "+
      "Serializer of type ${T} to XmlSeq")
  trait Serializer[-T] { serializer =>
    
    /** abstract method to specify the conversion to [[XmlSeq]] */
    def serialize(t: T): XmlSeq

    /** constructs a new [[Serializer]] from this, which pre-applies the [[fn]] function to
     *  the value prior to serialization */
    def contramap[T2](fn: T2 => T): Serializer[T2] = t => serializer.serialize(fn(t))
  }

  /** companion object to the [[Serializer]] typeclass interface */
  object Serializer {
    
    def fromMap(map: ListMap[String, XmlSeq])(implicit namespace: Namespace): XmlSeq =
      map.foldLeft(XmlSeq.Empty) {
        case (xml, (k, v: XmlSeq)) =>
          XmlSeq(xml.$root :+ Element(Name(namespace, k), Map(), v.$root))
      }
  }

  /** serializes bytes to XML text */
  implicit def byteSerializer: Serializer[Byte] = x => XmlSeq(Text(x.toString))

  /** serializes shorts to XML text */
  implicit def shortSerializer: Serializer[Short] = x => XmlSeq(Text(x.toString))

  /** serializes ints to XML text */
  implicit def intSerializer: Serializer[Int] = x => XmlSeq(Text(x.toString))
  
  /** serializes longs to XML text */
  implicit def longSerializer: Serializer[Long] = x => XmlSeq(Text(x.toString))
  
  /** serializes booleans to XML text, as the values "true" and "false" */
  implicit def booleanSerializer: Serializer[Boolean] = x => XmlSeq(Text(x.toString))

  /** serializes strings to XML text */
  implicit def stringSerializer: Serializer[String] = x => XmlSeq(Text(x))
  
  /** serializes floats to XML text */
  implicit def floatSerializer: Serializer[Float] = x => XmlSeq(Text(x.toString))
  
  /** serializes doubles to XML text */
  implicit def doubleSerializer: Serializer[Double] = x => XmlSeq(Text(x.toString))
  
  /** serializes [[BigDecimal]]s to XML text */
  implicit def bigDecimalSerializer: Serializer[BigDecimal] = x => XmlSeq(Text(x.toString))
  
  /** serializes [[BigInt]]s to XML text */
  implicit def bigIntSerializer: Serializer[BigInt] = x => XmlSeq(Text(x.toString))

  /** serializes [[None]]s to XML text */
  implicit def noneSerializer: Serializer[None.type] = x => XmlSeq(Text(""))
}

/** lower-priority serializer implicits */
trait XmlSeqSerializers_1 { this: XmlSeqSerializers =>

  /** serializes [[Option]]s to their value in the [[Some]] case, or an empty sequence
   *  otherwise */
  implicit def optionSerializer[T](implicit ser: Serializer[T]): Serializer[Option[T]] =
    _.map(ser.serialize).getOrElse(XmlSeq(Seq(), Vector()))
  
  /** serializes any traversible from the Scala collections library to an XML sequence,
   *  provided the element type can be serialized */
  implicit def traversableSerializer[Type, S[T] <: Traversable[T]](
      implicit serializer: Serializer[Type],
               tag: SeqTag[S[Type]],
               namespace: Namespace): Serializer[S[Type]] =
    _.foldLeft[XmlSeq](XmlSeq.Empty) { (xml, el) =>
      val elements = serializer.serialize(el).$root
      val node = Element(Name(namespace, tag.name), Map(), elements)
      XmlSeq(xml.$root :+ node)
    }
}

/** SAM typeclass interface for serialization to [[XmlNode]]s */
@implicitNotFound("Cannot serialize type ${T} to XmlNode. Please provide an implicit "+
  "Serializer of type ${T} to XmlNode.")
trait NodeSerializer[T] { nodeSerializer =>

  /** abstract method to specify the conversion to [[XmlNode]] */
  def serialize(t: T): XmlNode

  /** constructs a new [[NodeSerializer]] from this, which pre-applies the [[fn]] function to
    *  the value prior to serialization */
  def contramap[T2](fn: T2 => T): NodeSerializer[T2] =
    t => nodeSerializer.serialize(fn(t))
}

/** mixin providing serialization for [[XmlNode]]s */
trait XmlNodeSerializers {

  /** SAM typeclass interface for serialization to [[XmlNode]]s */
  @implicitNotFound("Cannot serialize type ${T} to XmlNode. Please provide an implicit "+
      "Serializer of type ${T} to XmlNode.")
  trait NodeSerializer[T] { nodeSerializer =>

    /** abstract method to specify the conversion to [[XmlNode]] */
    def serialize(t: T): XmlNode

    /** constructs a new [[NodeSerializer]] from this, which pre-applies the [[fn]] function to
     *  the value prior to serialization */
    def contramap[T2](fn: T2 => T): NodeSerializer[T2] =
      t => nodeSerializer.serialize(fn(t))
  }

  /** typeclass generator for providing a valid [[NodeSerializer]] if corresponding [[WrapTag]]
   *  and [[Serializer]] typeclass instances are available */
  implicit def seqToNode[T](implicit serializer: XmlSeq.Serializer[T],
                                     wrapTag: WrapTag[T],
                                     namespace: Namespace): NodeSerializer[T] = { (t: T) =>

    val children = serializer.serialize(t).$root
    XmlNode(Seq(Element(Name(namespace, wrapTag.name), Map(), children)), Vector())
  }
}
