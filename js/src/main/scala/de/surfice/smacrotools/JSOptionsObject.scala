// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Macro annotation to create JS "options objects" from case class definitions.
//
// Distributed under the MIT License (see included file LICENSE)
package de.surfice.smacrotools

import scala.annotation.{compileTimeOnly, StaticAnnotation}
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scala.scalajs.js

/**
 * This macro annotation transforms a case class into a trait extending js.Object
 * and a companion object with an apply() method. This allows easy definition
 * of JavaScript "confguration" objects, where some options are required, but ohters are optional.
 *
 * @example
 * {{{
 * @JSOptionsObject
 * case class FooOptions(var foo: Int, @JSName("notify") update: Boolean = false, bar: js.UndefOr[String] = js.undefined)
 * }}}
 * is transformed into
 * {{{
 * @js.native
 * trait FooOptions extends js.Object {
 *   var x: Int = js.native
 *
 *   @JSName("notify")
 *   val update: Boolean = js.native
 *
 *   val y: js.UndefOr[String] = js.native
 * }
 *
 * object FooOptions {
 *   def apply(foo: Int, update: Boolean = false, bar: js.UndefOr[String] = js.undefined): FooOptions =
 *     val __o = js.Dynamic.literal(x = x, notify = update).asInstanceOf[js.Dictionary[js.Any]]
 *
 *     if(bar.isDefined) __o.update("bar",bar)
 *
 *     __o.asInstanceOf[FooOptions]
 * }
 * }}}
 *
 * @note Arguments of type [[scalajs.js.UndefOr]] will only be defined on the returned object, iff their value is `!= js.undefined`
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class JSOptionsObject extends StaticAnnotation {

  def macroTransform(annottees: Any*): Any = macro JSOptionsObject.Macro.impl
}

object JSOptionsObject {

  private[smacrotools] class Macro(val c: whitebox.Context) extends JsWhiteboxMacroTools {
    private lazy val jsObjectType = c.weakTypeOf[js.Object]
    import c.universe._

    lazy val debug = isSet("de.surfice.smacrotools.debug")

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
        case ValDef(mods@JSName(jsname),argname,tpe,rhs) => (ValDef(mods,argname,tpe,rhs),jsname)
        case vdef: ValDef => (vdef,vdef.name)
      }
      val bodyMembers = members map {
        case (ValDef(mods,name,tpe,rhs),_) => ValDef(Modifiers(NoFlags,mods.privateWithin,mods.annotations),name,tpe,q"scalajs.js.native")
      }

      val (argsWithUndefinedDefault, otherArgs) = members.partition( _._1.rhs.toString.endsWith("js.undefined") )

      val literalArgs = otherArgs map ( p => q"${p._2} = ${p._1.name}" )
      val assignIfDefined = argsWithUndefinedDefault map ( p =>
        q"if(${p._1.name}.isDefined) __o.update(${p._2.toString},${p._1.name})")

      // TODO: better way than using a js.Dictionary?
      val tree =
        q"""{@scalajs.js.native
              trait $name extends scalajs.js.Object {
              ..$bodyMembers
              }
              object ${name.toTermName} {
                def apply(..${members map (_._1)}) = {
                  val __o = scalajs.js.Dynamic.literal(..$literalArgs).asInstanceOf[scalajs.js.Dictionary[scalajs.js.Any]]
                  ..$assignIfDefined
                  __o.asInstanceOf[$name]
                }
              }
            }"""

      if(debug) println(tree)

      c.Expr[Any](tree)
    }
  }
}
