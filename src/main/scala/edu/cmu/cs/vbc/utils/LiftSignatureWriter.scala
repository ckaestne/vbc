package edu.cmu.cs.vbc.utils

import edu.cmu.cs.vbc.vbytecode.{Owner, TypeDesc}
import org.objectweb.asm.signature.{SignatureVisitor, SignatureWriter}

/**
  * lift method signatures by using V (preserving generics) for all
  * arguments and return types, plus adding a new fexpr parameter
  *
  * Exception not supported yet. Array is lifted as conditional
  * array instead of array of conditional elements. May want to explore
  * different alternatives in the future.
  */
class LiftSignatureWriter() extends SignatureWriter() {

  import LiftUtils._

  //arrayStack stolen from TraceSignatureVisitor, see documentation there
  var arrayStack: Int = 0


  var inParam = false

  override def visitParameterType: SignatureVisitor = {
    if (inParam) {
      super.visitEnd()
    }


    val r = super.visitParameterType

    super.visitClassType(vclassname)
    super.visitTypeArgument('=')
    inParam = true


    return r
  }


  override def visitBaseType(descriptor: Char) {
    if (descriptor == 'V') {
      super.visitBaseType(descriptor)
    } else {
      val liftedClsType = LiftingPolicy.liftClassType(TypeDesc(primitiveToObjectType("" + descriptor)))
      val trimmed = liftedClsType.substring(1, liftedClsType.length - 1)
      visitClassType(trimmed)
      visitEnd()
    }
  }


  override def visitReturnType(): SignatureVisitor = {
    if (inParam)
      super.visitEnd()


    //add extra parameter at the end
    super.visitParameterType()
    super.visitClassType(fexprclassname)
    super.visitEnd()
    //now continue with return type
    super.visitReturnType()

    super.visitClassType(vclassname)
    super.visitTypeArgument('=')
  }

  override def visitInnerClassType(name: String): Unit = super.visitInnerClassType(LiftingPolicy.liftClassName(Owner(name)))

  override def visitClassType(name: String): Unit = super.visitClassType(LiftingPolicy.liftClassName(Owner(name)))

  def getSignature(): String =
    (this.toString + ">;").replace("Ledu/cmu/cs/varex/V<V>;", "V")

  //TODO
  override def visitExceptionType(): SignatureVisitor = {
    throw new RuntimeException("visitExceptionType() not supported")
  }
}
