// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Common utility functions for Scala.js blackbox and whitebox macros
//
// Distributed under the MIT License (see included file LICENSE)
package de.surfice.smacrotools

import scala.language.experimental.macros
import scala.language.reflectiveCalls

trait JsCommonMacroTools extends CommonMacroTools {
//  this: CommonMacroTools =>

  import c.universe._

  /**
   * Returns a Tree that represents the specified type `T` as a `js.Dynamic`.
   *
   * @tparam T
   */
  protected[this] def selectGlobalDynamic[T: c.WeakTypeTag] : Tree = selectGlobalDynamic( weakTypeOf[T].typeSymbol.fullName )

  /**
   * Returns a Tree that represents a type or object (specified by its fully qualified name) as a `js.Dynamic`.
   *
   * @param fullName
   */
  // TODO: move angulate2 specific code to angulate!
  protected[this] def selectGlobalDynamic(fullName: String) : Tree =
    selectDynamic(q"scalajs.js.Dynamic.global",fullName)
//    fullName.split("\\.").toList.foldLeft(q"scalajs.js.Dynamic.global":Tree)((b,name) => q"""$b.selectDynamic($name)""")

  protected[this] def selectDynamic(base: Tree, fullName: String) : Tree =
    fullName.split("\\.").toList.foldLeft(base)((b,name) => q"""$b.selectDynamic($name)""")

  /**
   * Returns a tree that represent the JS reference to a Scala.js type annotated with JSExport.
   *
   * @param fullName fully qualified name of the scala type
   */
  protected[this] def selectExported(fullName: String) : Tree =
    fullName.split("\\.").toList.foldLeft(q"scalajs.runtime.environmentInfo.exportsNamespace":Tree)((b,name) => q"""$b.selectDynamic($name)""")

  object JSName {
    def unapply(modifiers: Modifiers): Option[TermName] = modifiers.annotations.collectFirst {
      case q"new $name(..$params)" if name.toString.endsWith("JSName") => params.head
    } collect {
      case Literal(Constant(name: String)) => TermName(name)
    }
  }
}
