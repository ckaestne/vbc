package edu.cmu.cs.vbc.vbytecode

import javax.lang.model.SourceVersion

/**
  * Wrapper for method and field owner.
  *
  * In ASM library, owner is a class name represented by a String. By declaring it as a class,
  * we can avoid messing up with the order of owner, name and desc in cases like:
  *
  * {{{mv.visitMethodInsn(INVOKESPECIAL, owner, name, desc, itf)}}}
  *
  * We might also add some checking to ensure this is actually a valid class name.
  *
  * @author chupanw
  */
case class Owner(name: String) {
  require(SourceVersion.isName(name), "Invalid class name")
}

/** Wrapper for method name */
case class MethodName(name: String) {
  require(SourceVersion.isIdentifier(name), "Invalid method name")
}

/** Wrapper for field name */
case class FieldName(name: String) {
  require(SourceVersion.isIdentifier(name), "Invalid field name")
}

/** Wrapper for method description */
case class MethodDesc(desc: String) extends TypeVerifier {
  require(isValidMethod(desc), "Invalid method descriptor")
}

/** Wrapper for field description */
case class FieldDesc(desc: String) extends TypeVerifier {
  require(isValidType(desc), "Invalid field descriptor")
}

trait TypeVerifier {
  def isValidType(s: String): Boolean = s.size match {
    case 1 => s == "Z" || s == "C" || s == "B" || s == "S" || s == "I" || s == "F" || s == "J" || s == "D"
    case o if s.startsWith("L") && s.endsWith(";") => SourceVersion.isName(s.init.tail.replace('/', '.'))
    case array if s.startsWith("[") => isValidType(s.tail)
    case _ => false
  }

  def isValidParameterList(prefix: String, s: String): Boolean = {
    if (s.isEmpty) {
      prefix.isEmpty
    }
    else {
      if (isValidType(prefix + s.head))
        isValidParameterList("", s.tail)
      else
        isValidParameterList(prefix + s.head, s.tail)
    }
  }

  def isValidMethod(s: String): Boolean =
    if (s.startsWith("(") && s.contains(")")) {
      val split = s.tail.split(')')
      split.size == 2 && isValidParameterList("", split(0)) && (isValidType(split(1)) || split(1) == "V")
    }
    else
      false
}
