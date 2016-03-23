package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassVisitor, Handle, MethodVisitor, Type}

/**
  * @author chupanw
  */

trait FieldInstruction extends Instruction {
  def getFieldFromV(mv: MethodVisitor, env: VMethodEnv, owner: String, name: String, desc: String): Unit = {

    val VType = "Ledu/cmu/cs/varex/V;"
    val invokeName = "apply"
    val flatMapDesc = s"(Ljava/util/function/Function;)$VType"
    val flatMapOwner = "edu/cmu/cs/varex/V"
    val flatMapName = "flatMap"
    val lamdaFactoryOwner = "java/lang/invoke/LambdaMetafactory"
    val lamdaFactoryMethod = "metafactory"
    val lamdaFactoryDesc = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"

    val n = env.clazz.lambdaMethods.size
    def getLambdaFunName = "lambda$" + env.method.name.replace('<', '$').replace('>', '$') + "$" + n
    def getLambdaFunDesc = "(" + Type.getObjectType(owner) + ")" + VType
    def getInvokeType = {
      "()Ljava/util/function/Function;"
    }

    //TODO: deal with thisParameter
    mv.visitInvokeDynamicInsn(
      invokeName, // Method to invoke
      getInvokeType, // Descriptor for call site
      new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc), // Default LambdaFactory
      // Arguments:
      Type.getType("(Ljava/lang/Object;)" + "Ljava/lang/Object;"),
      new Handle(H_INVOKESTATIC, env.clazz.name, getLambdaFunName, getLambdaFunDesc),
      Type.getType("(" + Type.getObjectType(owner) + ")" + VType)
    )
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
      mv.visitVarInsn(ALOAD, 0) // load obj
      mv.visitFieldInsn(GETFIELD, owner, name, "Ledu/cmu/cs/varex/V;")
      mv.visitInsn(ARETURN)
      mv.visitMaxs(5, 5)
      mv.visitEnd()
    }

    env.clazz.lambdaMethods ::= lambda
  }

  def putFieldToV(mv: MethodVisitor, env: VMethodEnv, owner: String, name: String, desc: String): Unit = {

    val VType = "Ledu/cmu/cs/varex/V;"
    val invokeName = "apply"
    val flatMapDesc = s"(Ljava/util/function/Function;)$VType"
    val flatMapOwner = "edu/cmu/cs/varex/V"
    val flatMapName = "flatMap"
    val lamdaFactoryOwner = "java/lang/invoke/LambdaMetafactory"
    val lamdaFactoryMethod = "metafactory"
    val lamdaFactoryDesc = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"

    val n = env.clazz.lambdaMethods.size
    def getLambdaFunName = "lambda$" + env.method.name.replace('<', '$').replace('>', '$') + "$" + n
    def getLambdaFunDesc = s"($VType" + Type.getObjectType(owner) + ")" + VType
    def getInvokeType = {
      s"($VType)Ljava/util/function/Function;"
    }

    //TODO: deal with thisParameter
    mv.visitInvokeDynamicInsn(
      invokeName, // Method to invoke
      getInvokeType, // Descriptor for call site
      new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc), // Default LambdaFactory
      // Arguments:
      Type.getType("(Ljava/lang/Object;)" + "Ljava/lang/Object;"),
      new Handle(H_INVOKESTATIC, env.clazz.name, getLambdaFunName, getLambdaFunDesc),
      Type.getType("(" + Type.getObjectType(owner) + ")" + VType)
    )
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
      mv.visitVarInsn(ALOAD, 1) // load obj
      mv.visitVarInsn(ALOAD, 0)
      mv.visitFieldInsn(PUTFIELD, owner, name, "Ledu/cmu/cs/varex/V;")
      mv.visitInsn(ACONST_NULL)
      mv.visitInsn(ARETURN)
      mv.visitMaxs(5, 5)
      mv.visitEnd()
    }

    env.clazz.lambdaMethods ::= lambda
  }
}

/**
  * GETSTATIC
  *
  * @param owner
  * @param name
  * @param desc
  */
case class InstrGETSTATIC(owner: String, name: String, desc: String) extends FieldInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitFieldInsn(GETSTATIC, owner, name, desc)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      mv.visitFieldInsn(GETSTATIC, owner, name, "Ledu/cmu/cs/varex/V;")
    }
    else {
      mv.visitFieldInsn(GETSTATIC, owner, name, desc)
    }
  }
}

/**
  * PUTSTATIC
  *
  * @param owner
  * @param name
  * @param desc
  */
case class InstrPUTSTATIC(owner: String, name: String, desc: String) extends FieldInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitFieldInsn(PUTSTATIC, owner, name, desc)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    //TODO: work for println, but what if we are getting conditional?
    if (env.shouldLiftInstr(this))
      mv.visitFieldInsn(PUTSTATIC, owner, name, "Ledu/cmu/cs/varex/V;")
    else
      mv.visitFieldInsn(PUTSTATIC, owner, name, desc)
  }
}

/**
  * GETFIELD
  *
  * @param owner
  * @param name
  * @param desc
  */
case class InstrGETFIELD(owner: String, name: String, desc: String) extends FieldInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitFieldInsn(GETFIELD, owner, name, desc)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this))
      getFieldFromV(mv, env, owner, name, desc)
    else
      mv.visitFieldInsn(GETFIELD, owner, name, "Ledu/cmu/cs/varex/V;")
  }
}


/**
  * PUTFIELD
  *
  * @param owner
  * @param name
  * @param desc
  */
case class InstrPUTFIELD(owner: String, name: String, desc: String) extends FieldInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitFieldInsn(PUTFIELD, owner, name, desc)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.isConditionalField(owner, name, desc)) {
      /* Avoid reassigning to conditional fields */
      mv.visitInsn(POP)
      mv.visitInsn(POP)
    }
    else {
      /* At this point, stack should contain object reference and new value */

      /* store new value somewhere */
      val tmpVariable = env.freshLocalVar()
      mv.visitVarInsn(ASTORE, env.getVarIdx(tmpVariable))

      /* get the old value */
      mv.visitInsn(DUP)
      if (env.shouldLiftInstr(this))
        getFieldFromV(mv, env, owner, name, desc)
      else
        mv.visitFieldInsn(GETFIELD, owner, name, "Ledu/cmu/cs/varex/V;")

      /* put FE, new value and old value */
      loadFExpr(mv, env, env.getBlockVar(block))
      mv.visitInsn(SWAP)
      mv.visitVarInsn(ALOAD, env.getVarIdx(tmpVariable))
      mv.visitInsn(SWAP)
      callVCreateChoice(mv)

      if (env.shouldLiftInstr(this)) {
        putFieldToV(mv, env, owner, name, desc)
        mv.visitInsn(POP) //pop the dummy return values, which is required by flatMap
      }
      else
        mv.visitFieldInsn(PUTFIELD, owner, name, "Ledu/cmu/cs/varex/V;")
    }
  }
}