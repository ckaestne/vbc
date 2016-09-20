package edu.cmu.cs.vbc.vbytecode

import javax.lang.model.SourceVersion

import edu.cmu.cs.vbc.utils.VBCWrapper
import org.objectweb.asm.Type

/**
  * Wrapper for method and field owner.
  *
  * In ASM library, owner is a class name (or array type) represented by a String. By declaring it as a class,
  * we can avoid messing up with the order of owner, name and desc in cases like:
  *
  * {{{mv.visitMethodInsn(INVOKESPECIAL, owner, name, desc, itf)}}}
  *
  * We might also add some checking to ensure this is actually a valid class name or valid array type.
  *
  * @author chupanw
  */
case class Owner(name: String) extends TypeVerifier {
  require(isValidInternalName(name), s"Invalid Owner name: $name")

  override def equals(obj: scala.Any): Boolean = obj match {
    case o: Owner => name == o.name
    case s: String => name == s
    case _ => false
  }

  def isValidInternalName(s: String): Boolean = {
    if (s.startsWith("[")) {
      // array type
      isValidType(s.tail)
    }
    else {
      // Not array type, then it should be class
      // Should be a valid Java class name
      // Fully qualified name should be separated by "/" instead of "."
      !name.contains('.') && SourceVersion.isName(s.replace('/', '.'))
    }
  }

  def getTypeDesc: TypeDesc = TypeDesc(Type.getObjectType(name).getDescriptor)
}

/** Store implicit conversion to String, avoid changing too much existing code. */
object Owner {
  implicit def ownerToString(owner: Owner): String = owner.name

  def getInt = Owner("java/lang/Integer")

  def getShort = Owner("java/lang/Short")

  def getByte = Owner("java/lang/Byte")
  def getBoolean = Owner("java/lang/Boolean")
  def getString = Owner("java/lang/String")
}


/**
  * Wrapper for method name
  */
case class MethodName(name: String) {
  require(name == "<init>" || name == "<clinit>" || SourceVersion.isIdentifier(name), s"Invalid method name: $name")

  override def equals(obj: scala.Any): Boolean = obj match {
    case MethodName(s) => s == name
    case s: String => s == name
    case _ => false
  }
}

object MethodName {
  implicit def methodNameToString(m: MethodName): String = m.name
}


/**
  * Wrapper for field name
  */
case class FieldName(name: String) {
  require(SourceVersion.isIdentifier(name), s"Invalid field name: $name")

  override def equals(obj: scala.Any): Boolean = obj match {
    case FieldName(f) => f == name
    case s: String => name == s
    case _ => false
  }
}

object FieldName {
  implicit def fieldNameToString(f: FieldName): String = f.name
}


/**
  * Wrapper for method descriptor
  */
case class MethodDesc(descString: String) extends TypeVerifier {
  require(isValidMethod(descString), s"Invalid method descriptor: $descString")

  val mt = Type.getMethodType(descString)

  override def equals(obj: scala.Any): Boolean = obj match {
    case MethodDesc(md) => md == descString
    case s: String => descString == s
    case _ => false
  }

  def getArgCount: Int = mt.getArgumentTypes.size

  /** Return the return type in String format.
    *
    * Sometimes return type could be void, but void is not a valid TypeDesc.
    * For this reason, we return [[String]] instead of [[TypeDesc]]
    */
  def getReturnTypeString: String = mt.getReturnType().getDescriptor

  def getReturnTypeSort: Int = mt.getReturnType.getSort

  def isReturnVoid: Boolean = getReturnTypeString == "V"

  def getArgs: Array[TypeDesc] = Type.getMethodType(descString).getArgumentTypes.map(t => TypeDesc(t.getDescriptor))
}

object MethodDesc {
  implicit def methodDescToString(md: MethodDesc): String = md.descString
}


/**
  * Wrapper for field descriptor
  */
case class TypeDesc(desc: String) extends TypeVerifier {
  require(isValidType(desc), s"Invalid field descriptor: $desc")

  override def equals(obj: scala.Any): Boolean = obj match {
    case t: TypeDesc => desc == t.desc
    case s: String => desc == s
    case _ => false
  }

  def isPrimitive: Boolean = desc == "Z" || desc == "C" || desc == "B" || desc == "S" || desc == "I" || desc == "F" ||
    desc == "J" || desc == "D"

  def toObject: TypeDesc = desc match {
    case "Z" => TypeDesc("Ljava/lang/Boolean;")
    case "C" => TypeDesc("Ljava/lang/Char;")
    case "B" => TypeDesc("Ljava/lang/Byte;")
    case "S" => TypeDesc("Ljava/lang/Short;")
    case "I" => TypeDesc("Ljava/lang/Integer;")
    case "F" => TypeDesc("Ljava/lang/Float;")
    case "J" => TypeDesc("Ljava/lang/Long;")
    case "D" => TypeDesc("Ljava/lang/Double;")
    case _ => this
  }

  def castInt: TypeDesc = this match {
    case TypeDesc("Ljava/lang/Boolean;") => TypeDesc.getInt
    case TypeDesc("Ljava/lang/Char;") => TypeDesc.getInt
    case TypeDesc("Ljava/lang/Short;") => TypeDesc.getInt
    case TypeDesc("Ljava/lang/Byte;") => TypeDesc.getInt
    case _ => this
  }

  def toV: TypeDesc = TypeDesc("Ledu/cmu/cs/varex/V;")

  def isArray: Boolean = desc(0) == '['

  def getWrapper: TypeDesc = {
    val dimension = desc.lastIndexOf('[') + 1 // in case this is an array
    val baseType = TypeDesc(desc.substring(dimension))
    if (baseType.isPrimitive)
      TypeDesc(s"L${VBCWrapper.prefix}/" + "array/" * dimension + baseType.desc + ";")
    else
      TypeDesc(s"L${VBCWrapper.prefix}/" + "array/" * dimension + baseType.desc.init.tail + ";")
  }

  def getOwner: Option[Owner] =
    if (isArray || isPrimitive)
      None
    else
      Some(Owner(desc.tail.init))
}

object TypeDesc {
  implicit def typeDescToString(td: TypeDesc): String = td.desc

  def getInt: TypeDesc = TypeDesc("Ljava/lang/Integer;")

  def getString: TypeDesc = TypeDesc("Ljava/lang/String;")
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
