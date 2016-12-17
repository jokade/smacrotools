// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Common utility functions for blackbox and whitebox macros
//
// Distributed under the MIT License (see included file LICENSE)
package de.surfice.smacrotools

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
  protected[this] def isSet(flag: String) : Boolean = _settings.contains(flag)

  protected[this] def setting(flag: String, default: String): String = _settings.getOrElse(flag,default)

  protected[this] def printTree(tree: Tree) = {
    println( showCode(tree) )
  }

  protected[this] def error(msg: String) = c.error(c.enclosingPosition,msg)

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
   * Returns the tree for the first annotation of the specified type found on the symbol, or None.
   *
   * @param symbol Symbol to be searched for the specified annotation type
   * @param annotation Fully qualified name of the annotation type
   */
  protected[this] def findAnnotation(symbol: Symbol, annotation: String): Option[Tree] =
    findAnnotation(symbol.annotations.map(_.tree),annotation)

  // TODO: seems not to work correctly, if the specified annotation name does not match the actual name used for the annotation
  // (i.e. if the FQN was used as argument, but the simple name was used for the annotation)
  protected[this] def findAnnotation(annotations: Seq[Tree], annotation: String): Option[Tree] =
    annotations.collectFirst{
      case a @ q"new $name( ..$params )" if name.toString == annotation => a
      case a @ q"new $name()" if name.toString == annotation => a
    }

  /**
   * Returns a list of tuples containing all annotations found on the specified symbol.
   * The first element of each tuple is the fully qualified annotation name,
   * the second element represents the annotation tree.
   *
   * @param symbol type symbol to be parsed for annotations
   */
  protected[this] def findAnnotations(symbol: Symbol): Seq[(String,Tree)] =
    findAnnotations(symbol.annotations.map(_.tree))

  protected[this] def findAnnotations(annotations: Seq[Tree]): Seq[(String,Tree)] =
    annotations.collect{
      case a @ q"new $name( ..$params )" => (name.toString,a)
      case a @ q"new $name()" => (name.toString,a)
    }

  /**
   * Takes a tree representing an annotation value and a list with the names of all valid parameter names for this
   * annotation (in the correct order), and returns a map containing the tree for each specified parameter, or None
   * for unspecified parameters.
   *
   * @param annotation complete annotation tree
   * @param paramNames list with all allowed parameter names (in the correct order)
   *
   */
  // TODO: check if this function can be used for all (scala) annotations
  // TODO: change signature: 'None' will never be returned for an undefined parameter; instead we will recevie the default value
  protected[this] def extractAnnotationParameters(annotation: Tree, paramNames: Seq[String]) : Map[String,Option[Tree]] = annotation match {
    case q"new $name( ..$params )" =>
      if(paramNames.size < params.size)
        throw new Exception("received more annotation parameters than defined (check Seq passed to paramNames)!")
      else {
        c.typecheck(annotation.duplicate)
        val m = paramNames.map((_, None)).toMap[String, Option[Tree]] ++
          paramNames.zip(params).map({
            case (name, q"$p = $rhs") => (p.toString, Some(rhs))
            case (name, q"$rhs") => (name, Some(rhs))
          }).toMap
        assert( m.keys.forall( k => paramNames.contains(k) ))
        m
      }
    case q"new $name()" => paramNames.map( (_,None) ).toMap
    case _ => Map()
  }



  /**
   * Checks if the provided symbol is annotated with the specified type and returns the trees for
   * all arguments of this annotation.
   *
   * @param symbol Symbol to be checked for the annotation
   * @param annotation fully qualified name of the annotation
   * @param paramNames list with all allowed parameter names of the annotation (in the correct order)
   */
  protected[this] def extractAnnotationParameters(symbol: Symbol, annotation: String, paramNames: Seq[String]) : Option[Map[String,Option[Tree]]] =
    findAnnotation(symbol,annotation) map (t => extractAnnotationParameters(t,paramNames))

  /**
   * Takes a sequence of parameter definition trees and returns the corresponding parameter names.
   */
  protected[this] def paramNames(params: Iterable[Tree]): Iterable[TermName] = params map {
    case q"$mods val $name: $tpe = $rhs" => name
  }

  protected[this] def extractStringConstant(arg: Tree): Option[String] = arg match {
    case Constant(Literal(value)) => Some(value.toString)
    case Literal(Constant(value:String)) => Some(value)
    case x => println(x.getClass)
      None
  }

  /**
   * Returns the debug configuration from the provided modifiers
   * @param modifiers
   * @return
   */
  def getDebugConfig(modifiers: Modifiers): debug.DebugConfig = modifiers.annotations collectFirst {
    case d @ Apply((q"new debug",_)) =>
      val args = extractAnnotationParameters(d:Tree,Seq("showExpansion","logInstances"))
      debug.DebugConfig(
        booleanDebugArg(args,"showExpansion"),
        booleanDebugArg(args,"logInstances")
      )
  } getOrElse debug.defaultDebugConfig

  private def booleanDebugArg(args: Map[String,Option[Tree]], name: String): Boolean = args(name) match {
    case None => true
    case Some(q"false") => false
    case Some(q"true") => true
    case x => c.abort(c.enclosingPosition,s"Invalid value for @debug parameter $name: $x (must be true or false)")
  }

}

