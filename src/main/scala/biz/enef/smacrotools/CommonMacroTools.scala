// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Common utility functions for blackbox and whitebox macros
//
// Distributed under the MIT License (see included file LICENSE)
package biz.enef.smacrotools

import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros._

abstract class CommonMacroTools {
  type UniverseProvider = {
    val universe: Universe
  }
  val c: UniverseProvider with Internals with Aliases with Typers with Infrastructure with FrontEnds with Enclosures

  import c.universe._

  private lazy val _settings = c.settings.map(_.split("=") match {
    case Array(flag) => (flag,"")
    case Array(flag,value) => (flag,value)
  }).toMap

  /**
   * Returns true if the specified macro-setting is defined as an option to scalac
   *
   * @param flag
   */
  protected def isSet(flag: String) : Boolean = _settings.contains(flag)

  protected def printTree(tree: Tree) = {
    println( showCode(tree) )
  }

  /**
   * Returns the full path name of the enclosing package (at the current position),
   * or None, if there is no enclosing package.
   */
  protected[this] def getEnclosingNamespace() : Option[String] = c.internal.enclosingOwner.fullName match {
    case "<empty>" => None
    case s => Some(s)
  }

  /**
   * Takes a tree and returns the fully qualified name of its type.
   *
   * @param tree
   * @return
   */
  protected[this] def getQualifiedTypeName(tree: Tree) : String = c.typecheck(tree,c.TYPEmode).tpe.toString

  /**
   * Takes a tree representing an annotation value and a list with the names of all valid parameter names for this
   * annotation (in the correct order), and returns a map containing the tree for each specified parameter, or None
   * for unspecified parameters.
   *
   * @param tree complete annotation tree
   * @param paramNames list with all allowed parameter names (in the correct order)
   *
   */
  // TODO: check if this function can be used for all (scala) annotations
  protected[this] def extractAnnotationParameters(tree: Tree, paramNames: Seq[String]) : Map[String,Option[Tree]] = tree match {
    case q"new $name( ..$params )" =>
      if(paramNames.size < params.size)
        throw new Exception("received more annotation parameters than defined (check Seq passed to paramNames)!")
      else {
        c.typecheck(tree.duplicate)
        val m = paramNames.map((_, None)).toMap[String, Option[Tree]] ++
          paramNames.zip(params).map({
            case (name, q"$p = $rhs") => (p.toString, Some(rhs))
            case (name, q"$rhs") => (name, Some(rhs))
          }).toMap
        assert( m.keys.forall( k => paramNames.contains(k) ))
        m
      }
    case _ => Map()
  }
}

