// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Base class for Scala.js whitebox macros.
//
// Distributed under the MIT License (see included file LICENSE)
package de.surfice.smacrotools

abstract class JsWhiteboxMacroTools extends WhiteboxMacroTools with JsCommonMacroTools {
  import c.universe._

  def getJSBaseClass(parents: Iterable[c.Tree]) : Iterable[c.Tree] = parents match {
    case Seq(x) if x.toString == "scala.AnyRef" => Seq(tq"scalajs.js.Object")
    case x => x
  }

}
