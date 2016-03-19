package edu.cmu.cs.vbc.util

import org.objectweb.asm.signature.{SignatureVisitor, SignatureWriter}

/**
  * lift method signatures by using V (preserving generics) for all
  * arguments and return types, plus adding a new fexpr parameter
  *
  * Exception not supported yet. Array is lifted as conditional
  * array instead of array of conditional elements. May want to explore
  * different alternatives in the future.
  */
class LiftSignatureWriter() extends SignatureWriter() with LiftUtils {
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
            visitClassType(primitiveToObjectType("" + descriptor).drop(1).dropRight(1))
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


    def getSignature(): String =
        (this.toString + ">;").replace("Ledu/cmu/cs/varex/V<V>;", "V")

    //TODO
    override def visitExceptionType(): SignatureVisitor = {
        throw new RuntimeException("visitExceptionType() not supported")
    }
}
