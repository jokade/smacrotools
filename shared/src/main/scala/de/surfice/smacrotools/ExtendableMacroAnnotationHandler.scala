package de.surfice.smacrotools

abstract class ExtendableMacroAnnotationHandler extends ExtendableMacro {
  override def extensionClasses: Seq[String] = setting("smacrotools.extensions","") match {
    case "" => Nil
    case s => s.split(";")
  }
}
