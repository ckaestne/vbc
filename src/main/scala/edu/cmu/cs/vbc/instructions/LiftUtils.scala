package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Type}


trait LiftUtils {
    //    val liftedPackagePrefixes = Set("edu.cmu.cs.vbc.test", "edu.cmu.cs.vbc.prog")
    val vclassname = "edu/cmu/cs/varex/V"
    val fexprclassname = "de/fosd/typechef/featureexpr/FeatureExpr"
    val fexprFactoryClassName = "de/fosd/typechef/featureexpr/FeatureExprFactory"
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

    def pushConstant(mv: MethodVisitor, value: Int): Unit = {
        value match {
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

    def pushConstantFALSE(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKESTATIC, "de/fosd/typechef/featureexpr/FeatureExprFactory", "False", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", false)

    def pushConstantTRUE(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKESTATIC, "de/fosd/typechef/featureexpr/FeatureExprFactory", "True", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", false)

    def callFExprIsSatisfiable(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "isSatisfiable", "()Z", true)

    def callFExprIsContradiction(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "isContradiction", "()Z", true)

    def callFExprOr(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "or", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)

    def callFExprAnd(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "and", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)

    def callFExprNot(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "not", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", true)

    def storeFExpr(mv: MethodVisitor, env: MethodEnv, v: Variable) =
        mv.visitVarInsn(ASTORE, env.getVarIdx(v))

    def loadFExpr(mv: MethodVisitor, env: MethodEnv, v: Variable) =
        mv.visitVarInsn(ALOAD, env.getVarIdx(v))

    def storeV(mv: MethodVisitor, env: MethodEnv, v: Variable) =
        mv.visitVarInsn(ASTORE, env.getVarIdx(v))

    def loadV(mv: MethodVisitor, env: MethodEnv, v: Variable) =
        mv.visitVarInsn(ALOAD, env.getVarIdx(v))

    /**
      * precondition: plain reference on top of stack
      * postcondition: V reference on top of stack
      */
    def callVCreateOne(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKESTATIC, vclassname, "one", "(Ljava/lang/Object;)Ledu/cmu/cs/varex/V;", true)

    /**
      * precondition: feature expression and two V references on top of stack
      * postcondition: V reference on top of stack
      */
    def callVCreateChoice(mv: MethodVisitor) =
        mv.visitMethodInsn(INVOKESTATIC, vclassname, "choice", "(Lde/fosd/typechef/featureexpr/FeatureExpr;Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", true)

    def initCondField(mv: MethodVisitor, name: String, owner: String) = {
        mv.visitVarInsn(ALOAD, 0)
        mv.visitLdcInsn(name)
        mv.visitMethodInsn(INVOKESTATIC, fexprFactoryClassName, "createDefinedExternal", "(Ljava/lang/String;)Lde/fosd/typechef/featureexpr/SingleFeatureExpr;", false)
        mv.visitInsn(ICONST_1)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        callVCreateOne(mv)
        mv.visitInsn(ICONST_0)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        callVCreateOne(mv)
        callVCreateChoice(mv)
        mv.visitFieldInsn(PUTFIELD, owner, name, "Ledu/cmu/cs/varex/V;")
    }

    def JavaType(t: String): String = {
        t match {
            case "Z" => "Ljava/lang/Boolean;"
            case "C" => "Ljava/lang/Char;"
            case "B" => "Ljava/lang/Byte;"
            case "S" => "Ljava/lang/Short;"
            case "I" => "Ljava/lang/Integer;"
            case "F" => "Ljava/lang/Float;"
            case "J" => "Ljava/lang/Long;"
            case "D" => "Ljava/lang/Double;"
            case "O" => "Ljava/lang/Object;"
            case _ => t
        }
    }

    def VType = "Ledu/cmu/cs/varex/V;"

    def FEType = "Lde/fosd/typechef/featureexpr/FeatureExpr;"

  /**
    * Helper function to get the method descriptor
    * @param strs strs.last is the return type, the rest is parameter list
    * @return method descriptor according to ASM library standard
    */
    def getMtdDesc(strs: String*): String = {
        getParaList(strs.dropRight(1):_*) + strs.last
    }

    def getParaList(strs: String*): String = {

        def toList(array: String*): String = {
            if (array.isEmpty) ""
            else array(0) + toList(array.tail:_*)
        }

        "(" + toList(strs:_*) + ")"
    }
}