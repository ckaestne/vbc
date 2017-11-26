package edu.cmu.cs.vbc

import edu.cmu.cs.vbc.vbytecode._

trait LiftingPolicy2 {
  def liftCall(owner: Owner, name: MethodName, desc: MethodDesc) = true //TODO

  def shouldLiftField(owner: Owner, name: FieldName, desc: TypeDesc) = true //TODO


  // classname with slashes
  def liftClass(name: String): Boolean

}

object NoLiftPolicy extends LiftingPolicy2 {
  override def liftClass(name: String) = false
}

object TestNonVLiftPolicy extends  LiftingPolicy2 {

  //TODO lift everything except strings, because otherwise field-initializers must be moved to constructor
  //(can be fixed later, or might not be worth it to twiddle with non-V lifting)
  override def liftClass(name: String) =
    !(Set("java/lang/String", "java/lang/Object") contains name)

}