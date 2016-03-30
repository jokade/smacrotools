// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Common utility functions for Scala.js blackbox and whitebox macros
//
// Distributed under the MIT License (see included file LICENSE)
package de.surfice.smacrotools

import scala.language.experimental.macros
import scala.language.reflectiveCalls

trait JsCommonMacroTools {
  this: CommonMacroTools =>

  import c.universe._

  /**
   * Returns a Tree that represents the specified type `T` as a `js.Dynamic`.
   *
   * @tparam T
   */
  def selectGlobalDynamic[T: c.WeakTypeTag] : Tree = selectGlobalDynamic( weakTypeOf[T].typeSymbol.fullName )

  /**
   * Returns a Tree that represents a type or object (specified by its fully qualified name) as a `js.Dynamic`.
   *
   * @param fullName
   */
  // TODO: move angulate2 specific code to angulate!
  def selectGlobalDynamic(fullName: String) : Tree =
    (fullName.split("\\.").toList match {
      case "angulate2" :: xs => "ng" :: xs
      case xs => xs
    }).foldLeft(q"scalajs.js.Dynamic.global":Tree)((b,name) => q"""$b.selectDynamic($name)""")

//  def jsNameAnnotation(modifiers: Modifiers): Option[TermName] = modifiers.annotations.collectFirst{
//    case q"new $name(..$params)" if name.toString.endsWith("JSName") => params.head
//  }

  object JSName {
    def unapply(modifiers: Modifiers): Option[TermName] = modifiers.annotations.collectFirst {
      case q"new $name(..$params)" if name.toString.endsWith("JSName") => params.head
    } collect {
      case Literal(Constant(name: String)) => TermName(name)
    }
  }
}
