// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description:
//
// Distributed under the MIT License (see included file LICENSE)
package de.surfice.smacrotools

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scala.scalajs.js

@compileTimeOnly("enable macro paradise to expand macro annotations")
class JSOptionsObject extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro JSOptionsObject.Macro.impl
}

object JSOptionsObject {

  private[smacrotools] class Macro(val c: whitebox.Context) extends JsWhiteboxMacroTools {
    private lazy val jsObjectType = c.weakTypeOf[js.Object]
    import c.universe._

    def impl(annottees: c.Expr[Any]*) : c.Expr[Any] = annottees.map(_.tree).toList match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)
      case _ => c.abort(c.enclosingPosition, "Invalid annottee for @JSOptionsObject")
    }

    def modifiedDeclaration(classDecl: ClassDef) = {
      val parts = extractClassParts(classDecl)
      import parts._

      if(!isCase)
        c.abort(c.enclosingPosition,"Invalid annottee for @JSOptionsObject: only case classes are allowed")


      val members = params map {
        case q"$mods val $name: $tpe = $rhs" => (mods,"val",name,tpe,rhs)
        case q"$mods var $name: $tpe = $rhs" => (mods,"var",name,tpe,rhs)
      }
      val bodyMembers = members map {
        case (_,"val",name,tpe,_) => q"val $name: $tpe = scalajs.js.native"
        case (_,"var",name,tpe,_) => q"var $name: $tpe = scalajs.js.native"
      }
      val args = members map ( p => ValDef(p._1,p._3,p._4,p._5) )
      val literalArgs = members map ( p => q"${p._3} = ${p._3}" )

      val tree =
        q"""{@scalajs.js.native
              trait $name extends scalajs.js.Object {
              ..$bodyMembers
              }
              object ${name.toTermName} {
                def apply(..$args) = scalajs.js.Dynamic.literal(..$literalArgs).asInstanceOf[$name]
              }
            }"""

      c.Expr[Any](tree)
    }
  }
}
