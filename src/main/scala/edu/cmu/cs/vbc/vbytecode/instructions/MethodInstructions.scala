package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.utils.LiftingFilter
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassVisitor, Handle, MethodVisitor, Type}

/**
  * @author chupanw
  */

trait MethodInstruction extends Instruction {

  def invokeDynamic(owner: String, name: String, desc: String, itf: Boolean,
                    mv: MethodVisitor, env: VMethodEnv, block: Block,
                    isSpecial: Boolean): Unit = {
    val FEType = "Lde/fosd/typechef/featureexpr/FeatureExpr;"
    val VType = "Ledu/cmu/cs/varex/V;"
    val invokeName = "apply"
    val flatMapDesc = s"(Ljava/util/function/BiFunction;${FEType})$VType"
    val flatMapOwner = "edu/cmu/cs/varex/V"
    val flatMapName = "vflatMap"
    val lamdaFactoryOwner = "java/lang/invoke/LambdaMetafactory"
    val lamdaFactoryMethod = "metafactory"
    val lamdaFactoryDesc = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"

    val n = env.clazz.lambdaMethods.size
    def getLambdaFunName = "lambda$" + env.method.name + "$" + n
    def getLambdaFunDesc = "(" + VType * Type.getArgumentTypes(desc).size + FEType + Type.getObjectType(owner) + ")" + VType
    def isReturnVoid = Type.getReturnType(desc) == Type.VOID_TYPE
    def getInvokeType = {
      val nArg = Type.getArgumentTypes(desc).size
      "(" + VType * nArg + ")Ljava/util/function/BiFunction;"
    }

    mv.visitInvokeDynamicInsn(
      invokeName, // Method to invoke
      getInvokeType, // Descriptor for call site
      new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc), // Default LambdaFactory
      // Arguments:
      Type.getType("(Ljava/lang/Object;Ljava/lang/Object;)" + "Ljava/lang/Object;"),
      new Handle(H_INVOKESTATIC, env.clazz.name, getLambdaFunName, getLambdaFunDesc),
      Type.getType("(" + FEType + Type.getObjectType(owner) + ")" + VType)
    )
    if (env.isMain) pushConstantTRUE(mv) else loadFExpr(mv, env, env.getBlockVar(block))
    mv.visitMethodInsn(INVOKEINTERFACE, flatMapOwner, flatMapName, flatMapDesc, true)
    if (isReturnVoid) mv.visitInsn(POP)

    val lambda = (cv: ClassVisitor) => {
      val mv: MethodVisitor = cv.visitMethod(
        ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
        getLambdaFunName,
        getLambdaFunDesc,
        getLambdaFunDesc,
        Array[String]() // TODO: handle exception list
      )
      mv.visitCode()
      val nArg = Type.getArgumentTypes(desc).size
      mv.visitVarInsn(ALOAD, nArg + 1) // load obj
      for (i <- 0 until nArg) mv.visitVarInsn(ALOAD, i) // load arguments
      mv.visitVarInsn(ALOAD, nArg) // load ctx
      if (isSpecial)
        mv.visitMethodInsn(INVOKESPECIAL, owner, name, liftMethodDescription(desc), false)
      else
        mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, liftMethodDescription(desc), false)
      if (isReturnVoid) {
        mv.visitInsn(ACONST_NULL) // this is because flatMap requires some return values
        mv.visitInsn(ARETURN)
      } else mv.visitInsn(ARETURN)
      mv.visitMaxs(nArg + 2, nArg + 2)
      mv.visitEnd()
    }

    env.clazz.lambdaMethods ::= lambda
  }
}

/**
  * INVOKESPECIAL instruction
  *
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKESPECIAL(owner: String, name: String, desc: String, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESPECIAL, owner, name, desc, itf)
  }

  /**
    * Generate variability-aware method invocation. For method invocations, we always expect that arguments are V (this
    * limitation could be relaxed later with more optimization). Thus, method descriptors always need to be lifted.
    * Should lift or not depends on the object reference on stack. If the object reference (on which method is going to
    * invoke on) is not a V, we generate INVOKESPECIAL as it is. But if it is a V, we need INVOKEDYNAMIC to invoke
    * methods on all the objects in V.
    *
    * @param mv    MethodWriter
    * @param env   Super environment that contains all the transformation information
    * @param block Current block
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      invokeDynamic(owner, name, desc, itf, mv, env, block, true)
    }
    else {
      if (!LiftingFilter.shouldLiftMethod(owner, name, desc)) {
        // TODO: replacing arguments with object types is not enough
        mv.visitMethodInsn(INVOKESPECIAL, owner, name, replaceArgsWithObject(desc), itf)
      }
      else {
        if (env.isMain) pushConstantTRUE(mv) else loadFExpr(mv, env, env.getBlockVar(block))
        mv.visitMethodInsn(INVOKESPECIAL, owner, name, liftMethodDescription(desc), itf)
        if (env.needsWrapping(this)) callVCreateOne(mv)
      }
    }
  }

  /**
    * Used to identify the start of init method
    *
    * @see [[Rewrite.rewrite()]]
    */
  override def isINVOKESPECIAL_OBJECT_INIT: Boolean =
    owner == "java/lang/Object" && name == "<init>" && desc == "()V" && !itf
}


/**
  * INVOKEVIRTUAL instruction
  *
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKEVIRTUAL(owner: String, name: String, desc: String, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, desc, itf)
  }


  /**
    * Generate variability-aware method invocation. For method invocations, we always expect that arguments are V (this
    * limitation could be relaxed later with more optimization). Thus, method descriptors always need to be lifted.
    * Should lift or not depends on the object reference on stack. If the object reference (on which method is going to
    * invoke on) is not a V, we generate INVOKEVIRTUAL as it is. But if it is a V, we need INVOKEDYNAMIC to invoke
    * methods on all the objects in V.
    *
    * @note Although this looks very similar to the implementation of INVOKESPECIAL, one significant difference is that
    *       INVOKESPECIAL needs special hacking for object initialization. For INVOKESPECIAL, it is possible that we
    *       need to wrap the previously duplicated uninitialized object reference into a V, but it is never the case
    *       for INVOKEVIRTUAL.
    * @param mv    MethodWriter
    * @param env   Super environment that contains all the transformation information
    * @param block Current block
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {

    if (env.shouldLiftInstr(this)) {
      invokeDynamic(owner, name, desc, itf, mv, env, block, false)
    }
    else {
      if (!LiftingFilter.shouldLiftMethod(owner, name, desc)) {
        mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, replaceArgsWithObject(desc), itf)
      }
      else {
        if (env.isMain) pushConstantTRUE(mv) else loadFExpr(mv, env, env.getBlockVar(block))
        mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, liftMethodDescription(desc), itf)
      }
    }
  }
}


/**
  * INVOKESTATIC
  *
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKESTATIC(owner: String, name: String, desc: String, itf: Boolean) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESTATIC, owner, name, desc, itf)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    //TODO: better filtering
    if (isValueOf)
      return
    if (owner.startsWith("java/")) {
      mv.visitMethodInsn(INVOKESTATIC, owner, name, desc, itf)
    }

    loadFExpr(mv, env, env.getBlockVar(block))
    mv.visitMethodInsn(INVOKESTATIC, owner, name, liftMethodDescription(desc), itf)
  }

  /**
    * Every time we do a ICONST_N, we automatically wrap it with Integer and then a One,
    * so there is no need to invoke valueOf again.
    *
    * @todo same for Long, Short, Float, Double, Byte, etc.
    */
  def isValueOf = {
    owner == "java/lang/Integer" && name == "valueOf" && desc == "(I)Ljava/lang/Integer;"
  }
}