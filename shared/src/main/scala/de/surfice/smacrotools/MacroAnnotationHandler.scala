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

import scala.annotation.StaticAnnotation


abstract class MacroAnnotationHandler extends WhiteboxMacroTools {
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
        handleClassDef(classDef)
      case (moduleDef: ModuleDef) :: Nil if supportsObjects =>
        handleModuleDef(moduleDef)
      case (classDef: ClassDef) :: (moduleDef: ModuleDef) :: Nil if supportsClasses =>
        val classParts = extractTypeParts(classDef)
        val companionParts = extractObjectParts(moduleDef).copy(isCompanion = true)
        transformDef(TransformData(classParts,companionParts))
      case _ =>
        c.abort(c.enclosingPosition, s"Invalid annottee for @$annotationName")
        (EmptyTree,null)
    }

    val debugConfig = data("debugConfig").asInstanceOf[DebugConfig]
    if(debugConfig.showExpansion) c.info(c.enclosingPosition, showCode(tree), true)

    c.Expr[Any](tree)
  }

  def handleClassDef(cls: ClassDef, data: Data = Map()) = extractTypeParts(cls) match {
    case classParts: ClassParts if supportsClasses => transformDef(TransformData(classParts).addData(data))
    case traitParts: TraitParts if supportsTraits => transformDef(TransformData(traitParts).addData(data))
    case _ =>
      c.abort(c.enclosingPosition, s"invalid annottee for @$annotationName")
      (EmptyTree,null)
  }

  def handleModuleDef(obj: ModuleDef, data: Data = Map()) =
    transformDef( TransformData(extractObjectParts(obj)).addData(data) )

  sealed abstract class TransformData[+T<:CommonParts] {
    type U <: TransformData[T]
    def origParts: T
    def modParts: T
    def data: Data
    def addData(data: Data): U
    def updBody(body: Seq[Tree]): U
    def addStatements(stmts: Tree*): U = updBody(modParts.body ++ stmts)
    def updModifiers(modifiers: Modifiers): U
    def updAnnotations(annots: List[Tree]): U = updModifiers(Modifiers(modParts.modifiers.flags,modParts.modifiers.privateWithin,annots))
    def addAnnotations(annots: Tree*): U = updAnnotations(modParts.modifiers.annotations++annots)
    def updParents(parents: Seq[Tree]): U
  }
  sealed trait TypeTransformData[+T<:TypeParts] extends TransformData[T] {
    override type U <: TypeTransformData[T]
    def updCompanion(newCompanion: Option[ObjectParts]): U
    def addCompanionStatements(stmts: Tree*): U = updCompanion( modParts.companion map { comp =>
      comp.copy(body = comp.body ++ stmts)
    })
  }
  object TransformData {
    def apply(classParts: ClassParts): ClassTransformData = ClassTransformData(classParts,classParts,initData(classParts))
    def apply(traitParts: TraitParts): TraitTransformData = TraitTransformData(traitParts,traitParts,initData(traitParts))
    def apply(objectParts: ObjectParts): ObjectTransformData = ObjectTransformData(objectParts,objectParts,initData(objectParts))
    def apply(typeParts: TypeParts, objectParts: ObjectParts): TypeTransformData[TypeParts] = typeParts match {
      case cls: ClassParts =>
        val parts = cls.copy(companion = Some(objectParts))
        ClassTransformData(parts,parts,initData(parts))
      case trt: TraitParts =>
        val parts = trt.copy(companion = Some(objectParts))
        TraitTransformData(parts,parts,initData(parts))
    }
    def apply(typeParts: TypeParts): TypeTransformData[TypeParts] = typeParts match {
      case cls: ClassParts => apply(cls)
      case trt: TraitParts => apply(trt)
    }
  }
  case class ClassTransformData(origParts: ClassParts, modParts: ClassParts, data: Data) extends TypeTransformData[ClassParts] {
    type U = ClassTransformData
    override def addData(data: Data): ClassTransformData = this.copy(data = this.data ++ data)
    override def updBody(newBody: Seq[Tree]): ClassTransformData = this.copy(modParts = modParts.copy(body = newBody))
    override def updModifiers(modifiers: c.universe.Modifiers): ClassTransformData = copy(modParts = modParts.copy(modifiers=modifiers))
    override def updCompanion(newCompanion: Option[ObjectParts]): ClassTransformData = this.copy(modParts = modParts.copy(companion = newCompanion))
    override def updParents(parents: Seq[Tree]): ClassTransformData = this.copy(modParts = modParts.copy(parents=parents))
    def updCtorParams(params: Seq[Tree]): ClassTransformData = this.copy(modParts = modParts.copy(params=params))
    def updCtorParams(params: (Seq[Tree],Seq[Tree])): ClassTransformData = this.copy(modParts = modParts.copy(params=params._1, scndParams = params._2))
    def updCtorMods(ctorMods: Modifiers): ClassTransformData = this.copy(modParts = modParts.copy(ctorMods = ctorMods))
  }
  case class ObjectTransformData(origParts: ObjectParts, modParts: ObjectParts, data: Data) extends TransformData[ObjectParts] {
    type U = ObjectTransformData
    override def addData(data: Data): ObjectTransformData = this.copy(data = this.data ++ data)
    override def updModifiers(modifiers: c.universe.Modifiers): ObjectTransformData = copy(modParts = modParts.copy(modifiers=modifiers))
    override def updBody(newBody: Seq[Tree]): ObjectTransformData = this.copy(modParts = modParts.copy(body = newBody))
    override def updParents(parents: Seq[Tree]): ObjectTransformData = this.copy(modParts = modParts.copy(parents=parents))
  }
  case class TraitTransformData(origParts: TraitParts, modParts: TraitParts, data: Data) extends TypeTransformData[TraitParts] {
    type U = TraitTransformData
    override def addData(data: Data): TraitTransformData = this.copy(data = this.data ++ data)
    override def updBody(newBody: Seq[Tree]): TraitTransformData = this.copy(modParts = modParts.copy(body = newBody))
    override def updModifiers(modifiers: c.universe.Modifiers): TraitTransformData = copy(modParts = modParts.copy(modifiers=modifiers))
    override def updCompanion(newCompanion: Option[ObjectParts]): TraitTransformData = this.copy(modParts = modParts.copy(companion = newCompanion))
    override def updParents(parents: Seq[Tree]): TraitTransformData = this.copy(modParts = modParts.copy(parents=parents))
  }

  type Transformation = TransformData[CommonParts] => TransformData[CommonParts]
  type ClassTransformation = TransformData[CommonParts] => ClassTransformData
  type TypeTransformation = TransformData[CommonParts] => TypeTransformData[TypeParts]
//  type TraitTransformation = TransformData[CommonParts] => TraitTransformData
  type ObjectTransformation = TransformData[CommonParts] => ObjectTransformData
  type Analysis = Tuple2[CommonParts,Data] => Tuple2[CommonParts,Data]

  def transform: Transformation = x => x

  def analyze: Analysis = x => x


  private def transformObjectDef: ObjectTransformation = transform andThen {
    case obj: ObjectTransformData => obj
    case _ => ???
  }

  private def ensureCompanion: TypeTransformation = {
    case cls: TypeTransformData[_] =>
      val origParts = cls.origParts
      import origParts._
      cls.updCompanion((companion match {
        case None if createCompanion => Some(ObjectParts(name.toTermName,Modifiers(),Nil,Nil,fullName,true))
        case x => x
      }).map(parts => ObjectTransformData(parts,parts,cls.data)).map(transformObjectDef).map(_.modParts))
    case _ => ???
  }

  private def transformClassDef: Transformation = ensureCompanion andThen transform

  private val processedAnnotationType = weakTypeOf[MacroAnnotationHandler.processed]
  private val processedAnnotation = q"new de.surfice.smacrotools.MacroAnnotationHandler.processed"

  private def transformDef[T<:CommonParts](transformData: TransformData[T]): (Tree,Data) = {
    val isProcessed = hasAnnotation(transformData.origParts.modifiers.annotations, processedAnnotationType)
    val updTransformData = if(isProcessed) transformData else transformData.addAnnotations(processedAnnotation)
    updTransformData match {
      case cls: TypeTransformData[_] =>
        val tdata = if (isProcessed) cls else transformClassDef(cls)
        tdata match {
          case ClassTransformData(_, modParts, _) =>
            (classTree(modParts), tdata.data)
          case TraitTransformData(_, modParts, _) =>
            (traitTree(modParts), tdata.data)
          case _ => ???
        }
      case obj: ObjectTransformData =>
        val tdata = if (isProcessed) obj else transformObjectDef(obj)
        (objTree(tdata.modParts), tdata.data)
      case _ => ???
    }
  }

  private def classTree(cls: ClassParts): Tree = {
    import cls._
    if(scndParams.isEmpty)
      if(companion.isDefined)
        q"""{${modifiers} class $name[..$tparams] $ctorMods ( ..$params )
                       extends ..$parents { $self => ..$body }

                        ${objTree(companion.get)}
                       }"""
      else
        q"""${modifiers} class $name[..$tparams] $ctorMods ( ..$params )
                       extends ..$parents { $self => ..$body }"""
    else
      if(companion.isDefined)
        q"""{${modifiers} class $name[..$tparams] $ctorMods ( ..$params )( ..$scndParams )
                       extends ..$parents { $self => ..$body }

                        ${objTree(companion.get)}
                       }"""
      else
        q"""${modifiers} class $name[..$tparams] $ctorMods ( ..$params )( ..$scndParams )
                       extends ..$parents { $self => ..$body }"""
  }

  private def traitTree(trt: TraitParts): Tree = {
    import trt._
    if(companion.isDefined)
      q"""{${modifiers} trait $name[..$tparams]
                       extends ..$parents { $self => ..$body }

                        ${objTree(companion.get)}
                       }"""
    else
      q"""${modifiers} trait $name[..$tparams]
                       extends ..$parents { $self => ..$body }"""
  }

  private def objTree(obj: ObjectParts): Tree = {
    import obj._
    q"""$modifiers object $name extends ..$parents {
                      ..$body
                    }"""
  }

}

object MacroAnnotationHandler {
  class processed extends StaticAnnotation
}
