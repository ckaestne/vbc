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
    case "java/util/LinkedList" => true
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
        case (Owner("java/lang/Short"), _, _) => false
        case (Owner("java/lang/Byte"), _, _) => false
        case (Owner("java/lang/String"), _, _) => false
        case (Owner("java/io/PrintStream"), MethodName("println"), _) => false
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

  /** Lift the class name as specified in LiftingPolicy.
    *
    * If there is no need to lift this class, return original class name.
    */
  def liftClassName(owner: Owner): Owner = {

    def liftClsStr(owner: String): String = owner match {
      case cls if cls.startsWith("java") =>
        val lastSlash = owner.lastIndexOf('/')
        val vClsName = "/V" + owner.substring(lastSlash + 1)
        s"edu/cmu/cs/vbc/model/${owner.substring(5, lastSlash) + vClsName}"
      case array if array.startsWith("[") =>
        "[Ledu/cmu/cs/varex/V;" // all arrays are created as V
      //      s"[${liftClsStr(primitiveToObjectType(array.substring(1)))}"
      case _ => owner
    }

    //    Owner(liftClsStr(owner))
    owner // as of now we don't want to use any model classes.
  }

  /** Lift the class type as specified in LiftingPolicy.
    *
    * If there is no need to lift this class, return original class type.
    */
  def liftClassType(desc: TypeDesc): TypeDesc = {
    //    val t: Type = Type.getType(desc)
    //    t.getSort match {
    //      case Type.OBJECT =>
    //        desc match {
    //          case s if s.startsWith("Ljava") =>
    //            val lastSlashIdx = desc.lastIndexOf('/')
    //            val javaIdx = desc.indexOf("java")
    //            assert(lastSlashIdx != -1 && javaIdx != -1)
    //            TypeDesc(desc.substring(0, javaIdx) + "edu/cmu/cs/vbc/model/" + desc.substring(javaIdx + 5, lastSlashIdx) + "/V" + desc.substring(lastSlashIdx + 1))
    //          case _ => desc
    //        }
    //      case _ => desc
    //    }
    desc // as of now we don't want to use any model classes.
  }

}
