// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description:
//
// Distributed under the MIT License (see included file LICENSE)

//     Project: smacrotools
//      Module:
// Description:
package de.surfice.smacrotools

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.whitebox
import scala.language.experimental.macros

/** Creates a `@ScalaJSDefined` version of the annotated class or trait on the companion object.
 *
 * @example {{{
 * @createJS
 * trait Foo {
 *   def bar: String
 * }
 * }}}
 * will expand to {{{
 * trait Foo {
 *   def bar: String
 * }
 * object Foo {
 *   @js.native
 *   @ScalaJSDefined
 *   trait JS extends js.Object {
 *     def bar: String
 *   }
 * }
 * }}}
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class createJS extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro createJS.Macro.impl
}

object createJS {
  private[smacrotools] class Macro(val c: whitebox.Context) extends MacroAnnotationHandler {
    import c.universe._

    override def annotationName: String = "createJS"

    override def supportsClasses: Boolean = true
    override def supportsTraits: Boolean = true
    override def supportsObjects: Boolean = false
    override def createCompanion: Boolean = true

    override def transform: Transformation = super.transform andThen {
      case tpe: TypeTransformData[_] =>
        val parts = tpe.modParts
        import parts._
        val JS =
          if(isClass)
            q"""@scalajs.js.annotation.ScalaJSDefined
                class JS[..${tparams.asInstanceOf[Seq[Tree]]}] extends scalajs.js.Object {
                  ..${body.asInstanceOf[Seq[Tree]]}
                }"""
          else
            q"""@scalajs.js.annotation.ScalaJSDefined
                trait JS[..${tparams.asInstanceOf[Seq[Tree]]}] extends scalajs.js.Object {
                  ..${body.asInstanceOf[Seq[Tree]]}
                }"""
        tpe.updCompanion( companion.map{c =>
          val body = (c.body :+ JS)
          c.copy(body = body.asInstanceOf[Seq[Nothing]]).asInstanceOf[super.ObjectParts]
        })
      case default => default
    }

  }
}
