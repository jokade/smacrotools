// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Annotation to enable debugging of its annottee
//
// Distributed under the MIT License (see included file LICENSE)
package de.surfice.smacrotools

import scala.annotation.StaticAnnotation

/**
 * Enables debugging of an annotated angulate2 class during macro expansion and/or at runtime.
 *
 * @param showExpansion If true, the expanded macro code is logged during compilation
 * @param logInstances If true, log every instantiation during runtime
 */
class debug(showExpansion: Boolean = true,
            logInstances: Boolean = true) extends StaticAnnotation

object debug {
  case class DebugConfig(showExpansion: Boolean, logInstances: Boolean)
  private[smacrotools] lazy val defaultDebugConfig = DebugConfig(false,false)
}
