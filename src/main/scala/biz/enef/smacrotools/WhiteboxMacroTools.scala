// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Utility functions for whitebox macros
//
// Distributed under the MIT License (see included file LICENSE)
package biz.enef.smacrotools

import scala.reflect.macros.whitebox

abstract class WhiteboxMacroTools extends CommonMacroTools {
  val c: whitebox.Context
  import c.universe._

  /**
   * Extracts the name, constructor parameters, parent types (extends), and the body of the specified class declaration.
   *
   * @param classDecl
   * @return (name, params, parents, body)
   */
  def extractClassParts(classDecl: ClassDef) : (TypeName,Iterable[Tree],Iterable[Tree],Iterable[Tree]) = classDecl match {
    case q"class $className(..$fields) extends ..$parents { ..$body }" =>
      (className, fields, parents, body)
  }
}
