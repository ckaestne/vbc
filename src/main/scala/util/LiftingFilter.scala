package util

/**
  * Check whether it is necessary to lift a method or whether a method will be lifted
  * @author chupanw
  */
object LiftingFilter {
  def shouldLiftMethod(owner: String, name: String, desc: String): Boolean = {
    var should = true
    if (owner.startsWith("java")) should = true
    should
  }
}
