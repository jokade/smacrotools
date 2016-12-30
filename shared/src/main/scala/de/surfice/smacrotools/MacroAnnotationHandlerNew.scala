// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description:
//
// Distributed under the MIT License (see included file LICENSE)

// -   Project: smacrotools (https://github.com/jokade/smacrotools)
// Description:
//
// Distributed under the MIT License (see included file LICENSE)

//     Project: smacrotools
//      Module:
// Description:
package de.surfice.smacrotools

import de.surfice.smacrotools.debug.DebugConfig


abstract class MacroAnnotationHandlerNew extends WhiteboxMacroTools {
  import c.universe._
  type Data = Map[String,Any]
  def annotationName: String
  def supportsClasses: Boolean
  def supportsTraits: Boolean
  def supportsObjects: Boolean
  def createCompanion: Boolean

  private def initData(parts: CommonParts): Data = analyze( (parts, Map("debugConfig" -> getDebugConfig(parts.modifiers))) )._2

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val (tree,data) = annottees.map(_.tree).toList match {
      case (classDef: ClassDef) :: Nil if supportsClasses||supportsTraits =>
        extractTypeParts(classDef) match {
          case classParts: ClassParts if supportsClasses => transformDef(TransformData(classParts))
          case traitParts: TraitParts if supportsTraits => ???
          case _ =>
            c.abort(c.enclosingPosition, s"invalid annottee for @$annotationName")
            (EmptyTree,null)
        }
      case (moduleDef: ModuleDef) :: Nil if supportsObjects =>
        val objectParts = extractObjectParts(moduleDef)
        transformDef(TransformData(objectParts))
      case (classDef: ClassDef) :: (moduleDef: ModuleDef) :: Nil if supportsClasses =>
        val classParts = extractTypeParts(classDef).asInstanceOf[ClassParts]
        val companionParts = extractObjectParts(moduleDef).copy(isCompanion = true)
        transformDef(TransformData(classParts,companionParts))
      case _ =>
        c.abort(c.enclosingPosition, s"Invalid annottee for @$annotationName")
        (EmptyTree,null)
    }

    val debugConfig = data("debugConfig").asInstanceOf[DebugConfig]
    if(debugConfig.showExpansion) printTree(tree)

    c.Expr[Any](tree)
  }

  sealed abstract class TransformData[+T<:CommonParts] {
    type U <: TransformData[T]
    def origParts: T
    def modParts: T
    def data: Data
    def updBody(body: Seq[Tree]): U
    def addStatements(stmts: Tree*): U = updBody(modParts.body ++ stmts)
    def updModifiers(modifiers: Modifiers): U
    def updAnnotations(annots: List[Tree]): U = updModifiers(Modifiers(modParts.modifiers.flags,modParts.modifiers.privateWithin,annots))
    def addAnnotations(annots: Tree*): U = updAnnotations(modParts.modifiers.annotations++annots)
    def updParents(parents: Seq[Tree]): U
  }
  object TransformData {
    def apply(classParts: ClassParts): ClassTransformData = ClassTransformData(classParts,classParts,initData(classParts))
    def apply(objectParts: ObjectParts): ObjectTransformData = ObjectTransformData(objectParts,objectParts,initData(objectParts))
    def apply(classParts: ClassParts, objectParts: ObjectParts): ClassTransformData = {
      val parts = classParts.copy(companion = Some(objectParts))
      ClassTransformData(parts,parts,initData(parts))
    }
  }
  case class ClassTransformData(origParts: ClassParts, modParts: ClassParts, data: Data) extends TransformData[ClassParts] {
    type U = ClassTransformData
    def updBody(newBody: Seq[Tree]): ClassTransformData = this.copy(modParts = modParts.copy(body = newBody))
    override def updModifiers(modifiers: c.universe.Modifiers): ClassTransformData = copy(modParts = modParts.copy(modifiers=modifiers))
    def updCompanion(newCompanion: Option[ObjectParts]): ClassTransformData = this.copy(modParts = modParts.copy(companion = newCompanion))
    override def updParents(parents: Seq[Tree]): ClassTransformData = this.copy(modParts = modParts.copy(parents=parents))
  }
  case class ObjectTransformData(origParts: ObjectParts, modParts: ObjectParts, data: Data) extends TransformData[ObjectParts] {
    type U = ObjectTransformData
    override def updModifiers(modifiers: c.universe.Modifiers): ObjectTransformData = copy(modParts = modParts.copy(modifiers=modifiers))
    override def updBody(newBody: Seq[Tree]): ObjectTransformData = this.copy(modParts = modParts.copy(body = newBody))
    override def updParents(parents: Seq[Tree]): ObjectTransformData = this.copy(modParts = modParts.copy(parents=parents))
  }

  type Transformation = TransformData[CommonParts] => TransformData[CommonParts]
  type ClassTransformation = TransformData[CommonParts] => ClassTransformData
  type ObjectTransformation = TransformData[CommonParts] => ObjectTransformData
  type Analysis = Tuple2[CommonParts,Data] => Tuple2[CommonParts,Data]

  def transform: Transformation = x => x

  def analyze: Analysis = x => x


  private def transformObjectDef: ObjectTransformation = transform andThen {
    case obj: ObjectTransformData => obj
    case _ => ???
  }

  private def transformClassDef: ClassTransformation = transform andThen {
    case cls: ClassTransformData =>
      val origParts = cls.origParts
      import origParts._
      cls.updCompanion((companion match {
        case None if createCompanion => Some(ObjectParts(name.toTermName,modifiers,Nil,Nil,fullName,true))
        case x => x
      }).map(parts => ObjectTransformData(parts,parts,cls.data)).map(transformObjectDef).map(_.modParts))
    case _ => ???
  }

  def transformDef[T<:CommonParts](transformData: TransformData[T]): (Tree,Data) = transformData match {
    case cls: ClassTransformData =>
      val tdata = transformClassDef(cls)
      (classTree(tdata.modParts),tdata.data)
    case obj: ObjectTransformData =>
      val tdata = transformObjectDef(obj)
      (objTree(tdata.modParts),tdata.data)
    case _ => ???
  }

  private def classTree(cls: ClassParts): Tree = {
    import cls._
    if(companion.isDefined)
      q"""{${modifiers} class $name[..$tparams] $ctorMods ( ..$params )
                       extends ..$parents { $self => ..$body }

                        ${objTree(companion.get)}
                       }"""
    else
      q"""${modifiers} class $name[..$tparams] $ctorMods ( ..$params )
                       extends ..$parents { $self => ..$body }"""
  }

  private def objTree(obj: ObjectParts): Tree = {
    import obj._
    q"""$modifiers object $name extends ..$parents {
                      ..$body
                    }"""
  }

}
