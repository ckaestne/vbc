package edu.cmu.cs.vbc.utils

import edu.cmu.cs.vbc.vbytecode._

/**
  * Define lifting policy for methods and fields.
  *
  * In general, all classes in JDK should be lifted automatically. However, there are some
  * cases where default lifting is inefficient and we might need model classes. This object
  * defines which classes/methods/fields in JDK should be substituted, and which substitution
  * should we use.
  *
  * @todo Specify policies in configuration files or use drop-in.
  */
object LiftingPolicy {

  def shouldLiftClass(owner: Owner): Boolean = owner.name match {
    case x if x.startsWith("edu/cmu/cs/vbc/prog/") => true
    case x if x.endsWith("java/lang/StringBuilder") => true
    case x if x.endsWith("java/lang/AbstractStringBuilder") => true
    case _ => false
  }

  /**
    * Return true if we want to life this method call
    *
    * @todo return the actual model class name if we decided to lift this method call.
    */
  def shouldLiftMethodCall(owner: Owner, name: MethodName, desc: MethodDesc): Boolean = {
    if (shouldLiftClass(owner)) true
    else {
      (owner, name, desc) match {
        case (Owner("java/lang/Integer"), _, _) => false
        case (Owner("java/lang/Character"), _, _) => false
        case (Owner("java/lang/Short"), _, _) => false
        case (Owner("java/lang/Byte"), _, _) => false
        case (Owner("java/lang/String"), _, _) => false
        case (Owner("java/lang/Object"), _, _) => false
        case (Owner("java/io/PrintStream"), _, _) => false
        case (o, _, _) if o.name.endsWith("Exception") => false
        case _ => true
      }
    }
  }

  /**
    * Return true if we want to lift this field
    *
    * @todo return the actual model class name if we decided to lift this method call.
    */
  def shouldLiftField(owner: Owner, name: FieldName, desc: TypeDesc): Boolean = {
    (owner, name, desc) match {
      case (Owner("java/lang/System"), FieldName("out"), _) => false
      case _ => true
    }
  }
}
