package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.{VBCFrame, VBCType}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassVisitor, Handle, MethodVisitor, Type}

/**
  * @author chupanw
  */

/**
  * INVOKESPECIAL instruction
  *
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKESPECIAL(owner: String, name: String, desc: String, itf: Boolean) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESPECIAL, owner, name, desc, itf)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    // TODO: not complete, desc may need to be modified
    // TODO: if the method is invoked on a conditional object, we need VOP to do the invoke
    // TODO: better filering
    if (owner.startsWith("java/")) {
      mv.visitMethodInsn(INVOKESPECIAL, owner, name, desc, itf)
    }
    else {
      //TODO: solve this via some method call and stack analysis
      if (owner.endsWith("VInteger")) {
        mv.visitMethodInsn(INVOKESPECIAL, owner, name, liftMtdDescNoFE(desc), itf)
        callVCreateOne(mv)
        return
      }
      loadFExpr(mv, env, env.getBlockVar(block))
      mv.visitMethodInsn(INVOKESPECIAL, owner, name, liftMethodDescription(desc), itf)
    }
  }

  /**
    * Used to identify the start of init method
    *
    * @see [[Rewrite.rewrite()]]
    */
  override def isINVOKESPECIAL_OBJECT_INIT: Boolean =
    owner == "java/lang/Object" && name == "<init>" && desc == "()V" && !itf

  override def updateStack(s: VBCFrame) = {
    var stack = s.pop()._3 //L0
    for (j <- 0 until Type.getArgumentTypes(desc).length)
      stack = stack.pop()._3
    if (Type.getReturnType(desc) != Type.VOID_TYPE)
      stack = stack.push(VBCType(Type.getReturnType(desc)), this)
    stack
  }

}


/**
  * INVOKEVIRTUAL instruction
  *
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKEVIRTUAL(owner: String, name: String, desc: String, itf: Boolean) extends Instruction {

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, desc, itf)
  }

  val FEType = "Lde/fosd/typechef/featureexpr/FeatureExpr;"
  val VType = "Ledu/cmu/cs/varex/V;"
  val invokeName = "apply"
  val flatMapDesc = s"(Ljava/util/function/BiFunction;${FEType})$VType"
  val flatMapOwner = "edu/cmu/cs/varex/V"
  val flatMapName = "vflatMap"
  val lamdaFactoryOwner = "java/lang/invoke/LambdaMetafactory"
  val lamdaFactoryMethod = "metafactory"
  val lamdaFactoryDesc = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {

    //TODO: better filtering
    if (owner.startsWith("java/")) {
      // library code
      // TODO: implement a model class for StringBuilder
      if (owner == "java/lang/StringBuilder" && name == "append") {
        mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, genSign(primitiveToObjectType("O"), "Ljava/lang/StringBuilder;"), itf)
      }
      else if (owner == "java/io/PrintStream" && name == "println") {
        mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, genSign(primitiveToObjectType("O"), "V"), itf)
      }
      return
    }

    if (env.isMain) {
      loadFExpr(mv, env, env.getBlockVar(block))
      mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, liftMethodDescription(desc), false)
      return
    }

    val n = env.clazz.lambdaMethods.size
    def getLambdaFunName = "lambda$" + env.method.name + "$" + n
    def getLambdaFunDesc = "(" + VType * Type.getArgumentTypes(desc).size + FEType + Type.getObjectType(owner) + ")" +
      (if (isReturnVoid) "V" else VType)
    def isReturnVoid = Type.getReturnType(desc) == Type.VOID_TYPE
    def getInvokeType = {
      val nArg = Type.getArgumentTypes(desc).size
      "(" + VType * nArg + ")Ljava/util/function/BiFunction;"
    }

    //TODO: deal with thisParameter
    mv.visitInvokeDynamicInsn(
      invokeName, // Method to invoke
      getInvokeType, // Descriptor for call site
      new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc), // Default LambdaFactory
      // Arguments:
      Type.getType("(Ljava/lang/Object;Ljava/lang/Object;)" + (if (isReturnVoid) "V" else "Ljava/lang/Object;")),
      new Handle(H_INVOKESTATIC, env.clazz.name, getLambdaFunName, getLambdaFunDesc),
      Type.getType("(" + FEType + Type.getObjectType(owner) + ")" + (if (isReturnVoid) "V" else VType))
    )
    if (env.isMain) pushConstantTRUE(mv) else loadFExpr(mv, env, env.getBlockVar(block))
    mv.visitMethodInsn(INVOKEINTERFACE, flatMapOwner, flatMapName, flatMapDesc, true)

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
      mv.visitMethodInsn(INVOKEVIRTUAL, owner, name, liftMethodDescription(desc), false)
      if (isReturnVoid) mv.visitInsn(RETURN) else mv.visitInsn(ARETURN)
      mv.visitMaxs(nArg + 2, nArg + 2)
      mv.visitEnd()
    }

    env.clazz.lambdaMethods ::= lambda

  }

  override def updateStack(s: VBCFrame) = {
    var stack = s.pop()._3
    for (j <- 0 until Type.getArgumentTypes(desc).length)
      stack = stack.pop()._3
    if (Type.getReturnType(desc) != Type.VOID_TYPE)
      stack = stack.push(VBCType(Type.getReturnType(desc)), this)
    stack
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

  override def updateStack(s: VBCFrame) = {
    var stack = s
    for (j <- 0 until Type.getArgumentTypes(desc).length)
      stack = stack.pop()._3
    if (Type.getReturnType(desc) != Type.VOID_TYPE)
      stack = stack.push(VBCType(Type.getReturnType(desc)), this)
    stack
  }

}