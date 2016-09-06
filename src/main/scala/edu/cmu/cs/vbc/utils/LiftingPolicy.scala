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

  /**
    * Return true if we want to life this method call
    *
    * @todo return the actual model class name if we decided to lift this method call.
    */
  def shouldLiftMethodCall(owner: Owner, name: MethodName, desc: MethodDesc): Boolean = {
    (owner, name, desc) match {
      case (Owner("java/lang/Integer"), MethodName("valueOf"), _) => false
      case (Owner("java/lang/Integer"), MethodName("toString"), _) => false
      case (Owner("java/lang/Integer"), MethodName("<init>"), _) => false
      case _ => true
    }
  }

  /**
    * Return true if we want to lift this field
    *
    * @todo return the actual model class name if we decided to lift this method call.
    */
  def shouldLiftField(owner: Owner, name: FieldName, desc: TypeDesc): Boolean = {
    // as of now, we are lifting everything.
    true
  }

  /**
    * Return true if current JDK class is marked as [[Immutable]].
    *
    * Classes marked with [[Immutable]] are easier to lift because internal data will not changed once
    * objects are created. In such case, VClass is just a wrapper for Class.
    *
    */
  @deprecated
  def isImmutableCls(name: String): Boolean = {
    // Use reflection to check @Immutable annotation
    val cls = Class.forName(name)
    cls.getAnnotations.exists(_.isInstanceOf[edu.cmu.cs.varex.annotation.Immutable])
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
