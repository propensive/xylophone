package xylophone

import scala.reflect._

import macros._
import macrocompat.bundle

@bundle
private[xylophone] class XmlMacros(val c: whitebox.Context){

  import c.universe._

  def serializerMacro[T: c.WeakTypeTag]: c.Expr[XmlSeq.SeqSerializer[T]] = {
    val tpe = weakTypeOf[T].typeSymbol.asClass
    val serializer = typeOf[XmlSeq.SeqSerializer[_]].typeSymbol.asType.toTypeConstructor

    if (tpe.isCaseClass){
      weakTypeOf[T].decls.collect{
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.map{ param =>
        try {
          c.inferImplicitValue(appliedType(serializer, param.returnType), false, false)
        } catch {
          case e: Exception => println(e)
        }
      }
    }

    c.Expr[XmlSeq.SeqSerializer[T]](
      q"""
         new _root_.xylophone.XmlSeq.SeqSerializer[${weakTypeOf[T]}] {
             def serialize(obj: ${weakTypeOf[T]}): XmlSeq = ???
         }
       """)
  }
}
