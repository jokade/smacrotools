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
    def parents: Seq[Tree]
    def body: Seq[Tree]
    def modifiers: Modifiers
    def fullName: String
    def isClass: Boolean
    def isObject: Boolean
    def isCase: Boolean = modifiers.hasFlag(Flag.CASE)
    def isTrait: Boolean = modifiers.hasFlag(Flag.TRAIT)
    def isSubtypeOf(t: Type) : Boolean = parents.exists( p => c.typecheck(p,c.TYPEmode).tpe.<:<(t))
    val earlyDefns: Iterable[Tree] = Nil
  }

  sealed trait TypeParts extends CommonParts {
    def name: TypeName
    def tparams: Seq[Tree]
    def self: Tree
    def companion: Option[ObjectParts]
  }

  case class ClassParts(name: TypeName,
                        tparams: Seq[Tree],
                        params: Seq[Tree],
                        parents: Seq[Tree],
                        self: Tree,
                        body: Seq[Tree],
                        fullName: String,
                        modifiers: Modifiers,
                        ctorMods: Modifiers,
                        companion: Option[ObjectParts] = None) extends TypeParts {
    override val isObject = false
    override val isClass: Boolean = true
  }

  case class TraitParts(name: TypeName,
                        tparams: Seq[Tree],
                        params: Seq[Tree],
                        parents: Seq[Tree],
                        self: Tree,
                        body: Seq[Tree],
                        fullName: String,
                        modifiers: Modifiers,
                        companion: Option[ObjectParts] = None) extends TypeParts {
    override val isObject = false
    override val isClass: Boolean = false
  }

  case class ObjectParts(name: TermName,
                         modifiers: Modifiers,
                         parents: Seq[Tree],
                         body: Seq[Tree],
                         fullName: String,
                         isCompanion: Boolean = false) extends CommonParts {
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
  def extractTypeParts(classDef: ClassDef) : TypeParts = classDef match {
    case q"$mods class $className[..$tparams] $ctorMods(..$params) extends ..$parents { $self => ..$stats }" =>
      val fullName = getEnclosingNamespace().map( ns => s"$ns.$className" ).getOrElse(className.toString)
      ClassParts(className, tparams, params, parents, self, stats, fullName, mods, ctorMods)
    case q"$mods trait $traitName[..$tparams] extends ..$parents { $self => ..$stats }" =>
      val fullName = getEnclosingNamespace().map( ns => s"$ns.$traitName" ).getOrElse(traitName.toString)
      TraitParts(traitName, tparams, Nil, parents, self, stats, fullName, mods)

  }

  def extractObjectParts(moduleDef: ModuleDef): ObjectParts = moduleDef match {
    case q"$mods object $objName extends ..$parents { ..$body }" =>
      val fullName = getEnclosingNamespace().map(ns => s"$ns.$objName").getOrElse(objName.toString)
      ObjectParts(objName, mods, parents, body, fullName)
  }

}


