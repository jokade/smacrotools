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
   * @param tree Tree for which the type is to be evaluated
   * @param withMacrosDisabled set to true to avoid infinite loops when checking macros
   * @param dealias Set to true, to de-alias types
   * @return the fully qualified name of the type
   */
  protected[this] def getQualifiedTypeName(tree: Tree,
                                           withMacrosDisabled: Boolean = false,
                                           dealias: Boolean = false) : String = {
    val tpe = c.typecheck(tree,c.TYPEmode,withMacrosDisabled = withMacrosDisabled).tpe
    if(dealias)
      tpe.dealias.toString
    else
      tpe.toString
  }

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

  protected[this] def findAnnotationTypes(annotations: Seq[Tree]): Seq[(Type,Tree)] =
    annotations.collect{
      case a @ q"new $name( ..$params )" => (c.typecheck(name,c.TYPEmode).tpe,a)
      case a @ q"new $name()" => (c.typecheck(name,c.TYPEmode).tpe,a)
    }

  protected[this] def hasAnnotation(annotations: Seq[Tree], tpe: Type): Boolean = findAnnotationTypes(annotations)
    .find(_._1 <:< tpe).isDefined

  /**
   * Takes a tree representing an annotation value and a list with the names of all valid parameter names for this
   * annotation (in the correct order), and returns a map containing the tree for each specified parameter, or None
   * for unspecified parameters.
   *
   * @param annotation complete annotation tree
   * @param lastArgListNames list with all allowed parameter names for the last argument list (in the correct order)
   * @param firstArgListNames list with all allowed parameter names for the first argument list (if two arg lists are supported)
   *
   */
  // TODO: check if this function can be used for all (scala) annotations
  // TODO: change signature: 'None' will never be returned for an undefined parameter; instead we will recevie the default value
  protected[this] def extractAnnotationParameters(annotation: Tree, lastArgListNames: Seq[String], firstArgListNames: Seq[String] = Nil) : Map[String,Option[Tree]] = {
    def parseAnnotParams(params:Seq[Tree], paramNames: Seq[String]): Map[String,Option[Tree]] = {
      val vararg = paramNames.last.endsWith("*")
      if (paramNames.size < params.size && !vararg)
        throw new Exception("received more annotation parameters than defined (check Seq passed to paramNames; if the last param is a vararg, don't forget to append '*' to the param name)!")
      else {
        c.typecheck(annotation.duplicate)
        val simpleParamNames = if (vararg) paramNames.init else paramNames
        val m = paramNames.map((_, None)).toMap[String, Option[Tree]] ++
          simpleParamNames.zip(params).map({
            case (name, q"$p = $rhs") => (p.toString, Some(rhs))
            case (name, q"$rhs") => (name, Some(rhs))
          }).toMap ++
          (if (vararg) {
            val varargs = params.drop(simpleParamNames.size) map {
              case q"$_ = $rhs" => rhs
              case q"$rhs" => rhs
            }
            Map(paramNames.last.dropRight(1) -> Some(q"..$varargs"))
          } else Map())
        //        assert( m.keys.forall( k => paramNames.contains(k) ))
        m
      }
    }

    annotation match {
      case q"new $name( ..$params1 )( ..$params2 )" =>
        parseAnnotParams(params1,firstArgListNames) ++ parseAnnotParams(params2,lastArgListNames)
      case q"new $name( ..$params )" =>
        parseAnnotParams(params,lastArgListNames)
      case q"new $name()" => lastArgListNames.map( (_,None) ).toMap
      case _ => Map()
    }
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

  protected[this] def extractStringConstantSeq(arg: Tree): Seq[String] = arg match {
    case Constant(Literal(value)) => Seq(value.toString)
    case Literal(Constant(value:String)) => Seq(value)
    case Block(stats,expr) =>
      stats.flatMap(extractStringConstant) :+ extractStringConstant(expr).get
    case x => println(x.getClass)
      Nil
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

