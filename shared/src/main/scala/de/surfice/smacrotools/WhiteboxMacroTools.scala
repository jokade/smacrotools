// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description: Utility functions for whitebox macros
//
// Distributed under the MIT License (see included file LICENSE)
package de.surfice.smacrotools

import scala.reflect.macros.whitebox

abstract class WhiteboxMacroTools extends CommonMacroTools {
  val c: whitebox.Context
  import c.universe._

  sealed trait CommonParts {
    def parents: Iterable[Tree]
    def body: Iterable[Tree]
    def modifiers: Modifiers
    def fullName: String
    def isClass: Boolean
    def isCase: Boolean
    def isTrait: Boolean
    def isObject: Boolean
  }

  case class ClassParts(name: TypeName,
                        params: Iterable[Tree],
                        parents: Iterable[Tree],
                        body: Iterable[Tree],
                        fullName: String,
                        modifiers: Modifiers,
                        isClass: Boolean) extends CommonParts {
    override def isCase: Boolean = modifiers.hasFlag(Flag.CASE)
    override def isTrait: Boolean = modifiers.hasFlag(Flag.TRAIT)
    override val isObject = false
    def isSubtypeOf(t: Type) : Boolean = parents.exists( p => c.typecheck(p,c.TYPEmode).tpe.<:<(t))
  }

  case class ObjectParts(name: TermName,
                         modifiers: Modifiers,
                         parents: Iterable[Tree],
                         body: Iterable[Tree],
                         fullName: String) extends CommonParts {
    override val isClass: Boolean = false
    override val isCase: Boolean = false
    override val isTrait: Boolean = false
    override val isObject: Boolean = true
  }

  /**
   * Extracts the name, constructor parameters, parent types (extends), and the body of the specified class declaration.
   *
   * @param classDef
   */
  def extractClassParts(classDef: ClassDef) : ClassParts = classDef match {
    case q"$mods class $className(..$fields) extends ..$parents { ..$body }" =>
      val fullName = getEnclosingNamespace().map( ns => s"$ns.$className" ).getOrElse(className.toString)
      ClassParts(className, fields, parents, body, fullName, mods, true)
    case q"$mods trait $traitName extends ..$parents { ..$body }" =>
      val fullName = getEnclosingNamespace().map( ns => s"$ns.$traitName" ).getOrElse(traitName.toString)
      ClassParts(traitName, Nil, parents, body, fullName, mods, false)

  }

  def extractObjectParts(moduleDef: ModuleDef): ObjectParts = moduleDef match {
    case q"$mods object $objName extends ..$parents { ..$body }" =>
      val fullName = getEnclosingNamespace().map(ns => s"$ns.$objName").getOrElse(objName.toString)
      ObjectParts(objName, mods, parents, body, fullName)
  }

}
