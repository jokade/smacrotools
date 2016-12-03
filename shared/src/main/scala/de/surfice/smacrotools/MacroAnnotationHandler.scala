// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description:
//
// Distributed under the MIT License (see included file LICENSE)

//     Project: smacrotools
//      Module:
// Description:
package de.surfice.smacrotools

import scala.reflect.macros.whitebox

abstract class MacroAnnotationHandler(val c: whitebox.Context) extends WhiteboxMacroTools {
  import c.universe._

  def annotationName: String
  val supportsClasses = true
  val supportsObjects = false

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = annottees.map(_.tree).toList match {
    case (classDef: ClassDef) :: Nil if supportsClasses => modifiedClassDef(extractClassParts(classDef),classDef)
    case (moduleDef: ModuleDef) :: Nil if supportsObjects => modifiedModuleDef(extractObjectParts(moduleDef),moduleDef)
    case _ => c.abort(c.enclosingPosition, s"Invalid annottee for @$annotationName")
  }

  /**
   * Override this to modify the class declaration entirely with your own code.
   *
   * @param classParts
   * @param origClassDef
   * @return
   */
  def modifiedClassDef(classParts: ClassParts, origClassDef: ClassDef): Expr[Any] = {
    import classParts._

    val debug = getDebugConfig(modifiers)

    val tree = q"""
                   class $name( ..$params ) extends ..$parents {
                     ..$body
                   }"""

    if(debug.showExpansion) printTree(tree)

    c.Expr[Any](tree)
  }

  def modifiedAnnotations(parts: CommonParts): List[Tree] = {
    parts.modifiers.annotations
  }

  def modifiedFlags(parts: CommonParts): FlagSet = parts.modifiers.flags

  def modifiedModifiers(parts: CommonParts): Modifiers = {
    Modifiers(
      modifiedFlags(parts),
      parts.modifiers.privateWithin,
      modifiedAnnotations(parts)
    )
  }

  def modifiedModuleDef(objectParts: ObjectParts, origModuleDef:ModuleDef): Expr[Any] = {
    import objectParts._

    val debug = getDebugConfig(modifiers)

    val tree = q"""${modifiedModifiers(objectParts)} object $name extends ..$parents { ..$body }"""

    if(debug.showExpansion) printTree(tree)

    c.Expr[Any](tree)
  }
}
