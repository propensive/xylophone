package xylophone

import scala.reflect._

import macros._
import macrocompat.bundle

@bundle
private[xylophone] class XmlMacros(val c: whitebox.Context) {

  import c.universe._

  def serializerMacro[T: c.WeakTypeTag]: c.Expr[SeqSerializer[T]] = {
    val tpe = weakTypeOf[T].typeSymbol.asClass
    val serializer = typeOf[SeqSerializer[_]].typeSymbol.asType.toTypeConstructor

    println("serializer type: "+serializer)

    if(tpe.isCaseClass) {
      weakTypeOf[T].decls.collect {
        case m: MethodSymbol if m.isCaseAccessor => m.asMethod
      }.map { param =>
        println("Searching for parameter: "+param)
        //try {
          println(param.returnType)
          println(appliedType(serializer, param.returnType))
          c.inferImplicitValue(appliedType(serializer, param.returnType), false, false)
        //} catch {
        //  case e: Exception => println(e)
        //}
      }
    }

    c.Expr[SeqSerializer[T]](
      q"""
         new _root_.xylophone.SeqSerializer[${weakTypeOf[T]}] {
             def serialize(obj: ${weakTypeOf[T]}): XmlSeq = null
         }
       """)
  }
}
