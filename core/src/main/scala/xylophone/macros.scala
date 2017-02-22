package xylophone

import macrocompat.bundle

import scala.reflect.macros._

@bundle
private[xylophone] class XmlMacros(val c: whitebox.Context) {

  import c.universe._

  def serializerMacro[T: c.WeakTypeTag]: c.Expr[SeqSerializer[T]] = {
    val tpe = weakTypeOf[T].typeSymbol.asClass
    val serializer = typeOf[SeqSerializer[_]].typeSymbol.asType.toTypeConstructor
    if (tpe.isCaseClass) generateSerializerForCaseClass(serializer)
    else throw new Exception()
  }

  private def generateSerializerForCaseClass[T: c.WeakTypeTag](serializer: c.universe.Type): c.Expr[SeqSerializer[T]] = {
    val decls = getClassFields.map(field => findAndCollectSerializerForField(serializer, field))
    c.Expr[SeqSerializer[T]](
      q"""
         import scala.collection.immutable.ListMap
         new _root_.xylophone.SeqSerializer[${weakTypeOf[T]}] {
             def serialize(obj: ${weakTypeOf[T]}): XmlSeq =
                   _root_.xylophone.SeqSerializer.fromMap(ListMap(${decls.toSeq: _*}))
         }
       """)
  }

  private def findAndCollectSerializerForField[T: c.WeakTypeTag](serializer: c.universe.Type, param: c.universe.MethodSymbol) = {
    val implSerializer = c.inferImplicitValue(appliedType(serializer, param.returnType), silent = false, withMacrosDisabled = false)
    q"""(${param.name.toString} -> $implSerializer.serialize(obj.${param.name}))"""
  }

  private def getClassFields[T: c.WeakTypeTag]: Iterable[c.universe.MethodSymbol] = {
    weakTypeOf[T].decls.collect { case m: MethodSymbol if m.isCaseAccessor => m.asMethod }
  }

}
