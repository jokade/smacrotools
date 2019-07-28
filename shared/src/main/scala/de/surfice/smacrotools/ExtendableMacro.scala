package de.surfice.smacrotools

abstract class ExtendableMacro extends MacroAnnotationHandler {
  /// the names of all extension classes to be loaded
  def extensionClasses: Seq[String]

  val extensions: Seq[MacroAnnotationHandler] = extensionClasses map { cls =>
    getClass.getClassLoader.loadClass(cls).getConstructors.head.newInstance(c).asInstanceOf[MacroAnnotationHandler]
  }

  override def analyze: Analysis = extensions.foldLeft(super.analyze)((analyzer,extension) => analyzer andThen extension.analyze.asInstanceOf[Analysis])

  override def transform: Transformation = extensions.foldLeft(super.transform)((transformer,extension) => transformer andThen extension.transform.asInstanceOf[Transformation])
}
