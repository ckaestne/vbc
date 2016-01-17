package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Type}


trait LiftUtils {
    //    val liftedPackagePrefixes = Set("edu.cmu.cs.vbc.test", "edu.cmu.cs.vbc.prog")
    val vclassname = "edu/cmu/cs/varex/V"
    val fexprclassname = "de/fosd/typechef/featureexpr/FeatureExpr"
    val vopsclassname = "edu/cmu/cs/varex/VOps"
    val vclasstype = "L" + vclassname + ";"
    val ctxParameterOffset = 1

    //    protected def shouldLift(classname: String) = liftedPackagePrefixes.exists(classname startsWith _)

    protected def liftType(t: Type): String =
        if (t == Type.VOID_TYPE) t.getDescriptor
        else
            vclasstype

    protected def liftMethodDescription(desc: String): String = {
        val mtype = Type.getMethodType(desc)
        (Type.getObjectType(fexprclassname) +: mtype.getArgumentTypes.map(liftType)).mkString("(", "", ")") + liftType(mtype.getReturnType)
    }

    def writeConstant(mv: MethodVisitor, v: Int): Unit = {
        v match {
            case 0 => mv.visitInsn(ICONST_0)
            case 1 => mv.visitInsn(ICONST_1)
            case 2 => mv.visitInsn(ICONST_2)
            case 3 => mv.visitInsn(ICONST_3)
            case 4 => mv.visitInsn(ICONST_4)
            case 5 => mv.visitInsn(ICONST_5)
            case v if v <= Byte.MaxValue && v >= Byte.MinValue => mv.visitIntInsn(BIPUSH, v)
            //TODO other push operation for larger constants
        }
    }

    def writeConstantFALSE(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKESTATIC, "de/fosd/typechef/featureexpr/FeatureExprFactory", "False", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", false)

    def writeConstantTRUE(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKESTATIC, "de/fosd/typechef/featureexpr/FeatureExprFactory", "True", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", false)

    def writeFExprIsSatisfiable(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "isSatisfiable", "()Z", true)

    def writeFExprIsContradiction(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "isContradiction", "()Z", true)

    def writeFExprOr(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "or", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)

    def writeFExprAnd(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "and", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)

    def writeFExprNot(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "not", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", true)

    /**
      * precondition: plain reference on top of stack
      * postcondition: V reference on top of stack
      */
    def writeVCreateOne(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKESTATIC, vclassname, "one", "(Ljava/lang/Object;)Ledu/cmu/cs/varex/V;", true)

    /**
      * precondition: feature expression and two V references on top of stack
      * postcondition: V reference on top of stack
      */
    def writeVCreateChoice(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKESTATIC, vclassname, "choice", "(Lde/fosd/typechef/featureexpr/FeatureExpr;Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", true)



}