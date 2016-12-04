// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Annotation that expands to @JSImport or @JSName at compile time
//
// Distributed under the MIT License (see included file LICENSE)
package de.surfice.smacrotools

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.whitebox
import scala.language.experimental.macros

/**
 * This annotation will expand into either @JSName or @JSImport at compile time.
 *
 * @param global
 * @param cjsModule
 * @param cjsName
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class JSRef(global: String, cjsModule: String, cjsName: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro JSRef.Macro.impl
}

object JSRef {
   private[smacrotools] class Macro(val c: whitebox.Context) extends MacroAnnotationHandler with JsWhiteboxMacroTools {
     import c.universe._


     private val moduleMode = setting("smacrotools.jsref","global") match {
       case "global" => false
       case "cjsm" => true
       case x =>
         error(s"invalid compiler flag for 'smacrotools.jsref': $x (supported values are 'global' and 'cjsm'")
         false
     }

     override def annotationName: String = "JSRef"
     override def supportsClasses: Boolean = true
     override def supportsTraits: Boolean = true
     override def createCompanion: Boolean = false
     override def supportsObjects: Boolean = true

     override def modifiedAnnotations(parts: CommonParts, data: Data): (List[c.universe.Tree],Data) = {
       val params = extractAnnotationParameters(c.prefix.tree,Seq("global","cjsModule","cjsName"))
       val annot =
         if(moduleMode) q"""new scalajs.js.annotation.JSImport(${params("cjsModule").get},${params("cjsName").get})"""
         else q"""new scalajs.js.annotation.JSName(${params("global").get})"""

       (parts.modifiers.annotations :+ annot,data)
     }
   }

}
