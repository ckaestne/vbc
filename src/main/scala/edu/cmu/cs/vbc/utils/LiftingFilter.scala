package edu.cmu.cs.vbc.utils

/**
  * Check whether it is necessary to lift a method or whether a method will be lifted
  *
  * @author chupanw
  */
object LiftingFilter {

  def shouldLiftClass(clzName: String): Boolean = {
    var should = true
    clzName match {
      case java if clzName.startsWith("java") => should = false
      case _ => // do nothing
    }
    should
  }

  def shouldLiftMethod(owner: String, name: String, desc: String): Boolean = {
    var should = true
    owner match {
      case java if owner.startsWith("java") => should = false
      case _ => // do nothing
    }
    should
  }

  def shouldLiftField(owner: String, name: String, desc: String): Boolean = {
    var should = true
    owner match {
      case java if owner.startsWith("java") => should = false
      case _ => // do nothing
    }
    should
  }
}
