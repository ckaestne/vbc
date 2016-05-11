package edu.cmu.cs.vbc.utils

/**
  * Check whether it is necessary to lift a method or whether a method will be lifted
  *
  * @author chupanw
  */
object LiftingFilter {

  /**
    * Classes in java package should not be lifted
    * (otherwise JVM complains while loading lifted classes)
    *
    * More coming later, e.g. logging library
    */
  def shouldLiftMethod(owner: String, name: String, desc: String): Boolean = {
    owner match {
      case java if owner.startsWith("java") => !isImmutableCls(LiftUtils.liftCls(owner).replace('/', '.'))
      case _ => true
    }
  }

  /**
    * Should we lift this field?
    *
    * @param owner *original* owner name
    * @param name  field name
    * @param desc  field descriptor
    * @return true if should lift
    */
  def shouldLiftField(owner: String, name: String, desc: String): Boolean = {
    owner match {
      case java if owner.startsWith("java") => !isImmutableCls(LiftUtils.liftCls(owner).replace('/', '.'))
      case _ => true
    }
  }

  def isImmutableCls(name: String): Boolean = {
    // Use reflection to check @Immutable annotation
    val cls = Class.forName(name)
    cls.getAnnotations.exists(_.isInstanceOf[edu.cmu.cs.varex.annotation.Immutable])
  }
}
