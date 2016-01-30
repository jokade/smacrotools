// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Utility functions for whitebox macros
//
// Distributed under the MIT License (see included file LICENSE)
package biz.enef.smacrotools

import scala.reflect.macros.whitebox

abstract class WhiteboxMacroTools extends CommonMacroTools {
  val c: whitebox.Context
  import c.universe._

  case class ClassParts(name: TypeName,
                        params: Iterable[Tree],
                        parents: Iterable[Tree],
                        body: Iterable[Tree],
                        fullName: String,
                        modifiers: Modifiers,
                        isClass: Boolean) {
    def isCase = modifiers.hasFlag(Flag.CASE)
  }

  /**
   * Extracts the name, constructor parameters, parent types (extends), and the body of the specified class declaration.
   *
   * @param classDecl
   */
  def extractClassParts(classDecl: ClassDef) : ClassParts = classDecl match {
    case q"$mods class $className(..$fields) extends ..$parents { ..$body }" =>
      val fullName = getEnclosingNamespace().map( ns => s"$ns.$className" ).getOrElse(className.toString)
      ClassParts(className, fields, parents, body, fullName, mods, true)
    case q"$mods trait $traitName extends ..$parents { ..$body }" =>
      val fullName = getEnclosingNamespace().map( ns => s"$ns.$traitName" ).getOrElse(traitName.toString)
      ClassParts(traitName, Nil, parents, body, fullName, mods, false)

  }
}
