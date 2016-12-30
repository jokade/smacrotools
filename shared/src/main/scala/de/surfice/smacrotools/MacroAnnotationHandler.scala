// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description:
//
// Distributed under the MIT License (see included file LICENSE)

//     Project: smacrotools
//      Module:
// Description:
package de.surfice.smacrotools

import de.surfice.smacrotools.debug.DebugConfig


@deprecated("Use MacroAnnotationHandlerNew instead","0.0.4")
abstract class MacroAnnotationHandler extends WhiteboxMacroTools {
  import c.universe._
  type Data = Map[String,Any]
  def annotationName: String
  def supportsClasses: Boolean
  def supportsTraits: Boolean
  def supportsObjects: Boolean
  def createCompanion: Boolean

  private def initData(parts: CommonParts): Data = Map("debugConfig" -> getDebugConfig(parts.modifiers))

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val (tree,data) = annottees.map(_.tree).toList match {
      case (classDef: ClassDef) :: Nil if supportsClasses||supportsTraits =>
        extractTypeParts(classDef) match {
          case classParts: ClassParts if supportsClasses => modifiedClassDef(classParts,initData(classParts))
          case traitParts: TraitParts if supportsTraits => modifiedTraitDef(traitParts,initData(traitParts))
          case _ => c.abort(c.enclosingPosition, s"invalid annottee for @$annotationName")
        }
      case (moduleDef: ModuleDef) :: Nil if supportsObjects =>
        val objectParts = extractObjectParts(moduleDef)
        modifiedObjectDef(objectParts,initData(objectParts))
      case (classDef: ClassDef) :: (moduleDef: ModuleDef) :: Nil if supportsClasses =>
        val classParts = extractTypeParts(classDef).asInstanceOf[ClassParts]
        val companionParts = extractObjectParts(moduleDef).copy(isCompanion = true)
        modifiedClassDef(classParts.copy(companion = Some(companionParts)),initData(classParts))
      case _ => c.abort(c.enclosingPosition, s"Invalid annottee for @$annotationName")
    }

    val debugConfig = data("debugConfig").asInstanceOf[DebugConfig]
    if(debugConfig.showExpansion) printTree(tree)

    c.Expr[Any](tree)
  }

  /**
   * Override this to modify the class declaration entirely with your own code.
   *
   * @param classParts
   * @return
   */
  def modifiedClassDef(classParts: ClassParts, data: Data): (Tree,Data) = {
    import classParts._

    val (mods,data1) = modifiedModifiers(classParts,data)
    val (body,data2) = modifiedBody(classParts,data1)
    val (modParents,data3) = modifiedParents(classParts,data2)
    (companion match {
      case None if createCompanion => Some(ObjectParts(name.toTermName,modifiers,Nil,Nil,fullName,true))
      case x => x
    }).map( p => modifiedObjectDef(p,data3) ) match {
      case Some((comp,data)) =>
        val tree = q"""{${mods} class $name[..$tparams] $ctorMods ( ..$params )
                       extends ..$modParents { $self => ..$body }

                        $comp
                       }"""
        (tree,data)
      case None =>
        val tree = q"""${mods} class $name[..$tparams] $ctorMods ( ..$params )
                       extends ..$modParents { $self => ..$body }"""
        (tree,data3)
    }
  }

  def modifiedTraitDef(traitParts: TraitParts, data: Data): (Tree,Data) = {
    import traitParts._

    val (mods,data1) = modifiedModifiers(traitParts,data)
    val (body,data2) = modifiedBody(traitParts,data1)
    val (modParents,data3) = modifiedParents(traitParts,data2)
//    val comp = companion.map( p => modifiedObjectDef(p,origCompanionDef.get) ).getOrElse(EmptyTree)

    val tree = q"""..${mods.annotations}
                   trait $name[..$tparams] extends ..$modParents {
                     $self => ..$body
                   }
                 """
    (tree,data3)
  }

  def modifiedObjectDef(objectParts: ObjectParts, data: Data): (Tree,Data) = {
    import objectParts._

    val (mods,data1) = modifiedModifiers(objectParts,data)
    val (body,data2) = modifiedBody(objectParts,data1)
    val (modParents,data3) = modifiedParents(objectParts,data2)

    val tree = q"""$mods object $name extends ..$modParents {
                      ..$body
                    }"""

    (tree,data3)
  }

  def normalizeParents(parents: Seq[Tree]): Seq[Tree] = parents.map( p => (p.toString,p) ).toMap.values.toSeq

  def modifiedParents(parts: CommonParts, data: Data): (Seq[Tree],Data) = (parts.parents,data)

  def modifiedAnnotations(parts: CommonParts, data: Data): (List[Tree],Data) = {
    (parts.modifiers.annotations,data)
  }

  def modifiedFlags(parts: CommonParts, data: Data): (FlagSet,Data) = (parts.modifiers.flags,data)

  def modifiedModifiers(parts: CommonParts, data: Data): (Modifiers,Data) = {
    val (flags,data1) = modifiedFlags(parts,data)
    val (annots,data2) = modifiedAnnotations(parts,data1)
    (Modifiers(
      flags,
      parts.modifiers.privateWithin,
      annots
    ),data2)
  }

  def modifiedBody(parts: CommonParts, data: Data): (Iterable[Tree],Data) = (parts.body,data)
}
