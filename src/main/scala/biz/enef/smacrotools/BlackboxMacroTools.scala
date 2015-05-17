// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Utility functions for blackbox macros
//
// Distributed under the MIT License (see included file LICENSE)
package biz.enef.smacrotools

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

abstract class BlackboxMacroTools extends CommonMacroTools {
  val c: blackbox.Context
  import c.universe._

}
