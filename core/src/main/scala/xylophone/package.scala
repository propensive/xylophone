import language.experimental.macros
package object xylophone {
  implicit def xmlSerializerMacro[T]: XmlSeq.SeqSerializer[T] = macro XmlMacros.serializerMacro[T]
}
