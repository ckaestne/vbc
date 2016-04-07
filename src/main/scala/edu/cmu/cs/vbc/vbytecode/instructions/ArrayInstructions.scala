package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{INT_TYPE, REF_TYPE, VBCFrame, V_TYPE}
import edu.cmu.cs.vbc.vbytecode.{Block, MethodEnv, VMethodEnv}
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import edu.cmu.cs.vbc.utils.LiftUtils._

/**
  * Our first attempt is to implement array as array of V. If array length is different, then arrayref itself
  * is also a V.
  */
trait ArrayInstructions extends Instruction {
  def createVArray(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {

    val mapDesc = s"(Ljava/util/function/Function;$fexprclasstype)$vclasstype"
    val mapOwner = "edu/cmu/cs/varex/V"
    val mapName = "smap"

    def getLambdaFunName = "lambda$anewarray$"
    def getLambdaFunDesc = s"(Ljava/lang/Integer;)[$vclasstype"
    def getInvokeType = {
      "()Ljava/util/function/Function;"
    }

    mv.visitInvokeDynamicInsn(
      "apply", // Method to invoke
      getInvokeType, // Descriptor for call site
      new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc), // Default LambdaFactory
      // Arguments:
      Type.getType("(Ljava/lang/Object;)Ljava/lang/Object;"),
      new Handle(H_INVOKESTATIC, env.clazz.name, getLambdaFunName, getLambdaFunDesc),
      Type.getType(s"(Ljava/lang/Integer;)[$vclasstype")
    )
    loadCurrentCtx(mv, env, block)
    mv.visitMethodInsn(INVOKEINTERFACE, mapOwner, mapName, mapDesc, true)

    if (!(env.clazz.lambdaMethods contains getLambdaFunName)) {
      val lambda = (cv: ClassVisitor) => {
        val mv: MethodVisitor = cv.visitMethod(
          ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
          getLambdaFunName,
          getLambdaFunDesc,
          getLambdaFunDesc, // TODO: signature here ignores the type inside V
          Array[String]() // TODO: handle exception list
        )
        mv.visitCode()
        mv.visitVarInsn(ALOAD, 0)
        // It is possible that as this point we have a null on stack because of uninitialized field,
        // which is abnormal behavior in the source code. If that's the case, we expect a NullPointerException.

        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)  // JVM specification requires an int
        mv.visitTypeInsn(ANEWARRAY, "edu/cmu/cs/varex/V")
        mv.visitInsn(ARETURN)
        mv.visitMaxs(5, 5)
        mv.visitEnd()
      }
      env.clazz.lambdaMethods += (getLambdaFunName -> lambda)
    }
  }

  def storeOperation(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {

    val mtdDesc = s"(Ljava/util/function/BiConsumer;$fexprclasstype)V"
    val mtdOwner = "edu/cmu/cs/varex/V"
    val mtdName = "sforeach"

    def getLambdaFunName = "lambda$aastore_ref$"
    def getLambdaFunDesc = s"($vclasstype$vclasstype$fexprclasstype[$vclasstype)V"
    def getInnerLambdaFunName = "lambda$aastore_idx$"
    def getInnerLambdaFunDesc = s"([$vclasstype${vclasstype}Ljava/lang/Integer;)V"

    mv.visitInvokeDynamicInsn(
      "accept", // Method to invoke
      s"($vclasstype$vclasstype)Ljava/util/function/BiConsumer;", // Descriptor for call site
      new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc), // Default LambdaFactory
      // Arguments:
      Type.getType("(Ljava/lang/Object;Ljava/lang/Object;)V"),
      new Handle(H_INVOKESTATIC, env.clazz.name, getLambdaFunName, getLambdaFunDesc),
      Type.getType(s"($fexprclasstype[$vclasstype)V")
    )
    loadCurrentCtx(mv, env, block)
    mv.visitMethodInsn(INVOKEINTERFACE, mtdOwner, mtdName, mtdDesc, true)

    if (!(env.clazz.lambdaMethods contains getLambdaFunName)) {
      val lambda = (cv: ClassVisitor) => {
        val mv: MethodVisitor = cv.visitMethod(
          ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
          getLambdaFunName,
          getLambdaFunDesc,
          getLambdaFunDesc, // TODO: signature here ignores the type inside V
          Array[String]() // TODO: handle exception list
        )
        mv.visitCode()
        mv.visitVarInsn(ALOAD, 0)   // index

        mv.visitVarInsn(ALOAD, 3)   // array reference
        mv.visitVarInsn(ALOAD, 1)   // value
        mv.visitInvokeDynamicInsn(
          "accept",
          s"([$vclasstype$vclasstype)Ljava/util/function/Consumer;", // Descriptor for call site
          new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc),
          // Arguments:
          Type.getType("(Ljava/lang/Object;)V"),
          new Handle(H_INVOKESTATIC, env.clazz.name, getInnerLambdaFunName, getInnerLambdaFunDesc),
          Type.getType("(Ljava/lang/Integer;)V")
        )
        mv.visitVarInsn(ALOAD, 2) // load ctx
        mv.visitMethodInsn(INVOKEINTERFACE,
          "edu/cmu/cs/varex/V",
          "sforeach",
          s"(Ljava/util/function/Consumer;$fexprclasstype)V",
          true
        )
        mv.visitInsn(RETURN)
        mv.visitMaxs(5, 5)
        mv.visitEnd()
      }

      env.clazz.lambdaMethods += (getLambdaFunName -> lambda)

      if (!(env.clazz.lambdaMethods contains getInnerLambdaFunName)) {
        val innerLambda = (cv: ClassVisitor) => {
          val mv: MethodVisitor = cv.visitMethod(
            ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
            getInnerLambdaFunName,
            getInnerLambdaFunDesc,
            getInnerLambdaFunDesc,
            Array[String]()
          )
          mv.visitCode()
          mv.visitVarInsn(ALOAD, 0)   // array ref
          mv.visitVarInsn(ALOAD, 2)   // index
          mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)
          mv.visitVarInsn(ALOAD, 1)   // value
          mv.visitInsn(AASTORE)
          mv.visitInsn(RETURN)
          mv.visitMaxs(5, 5)
          mv.visitEnd()
        }
        env.clazz.lambdaMethods += (getInnerLambdaFunName -> innerLambda)
      }
    }
  }

  def loadOperation(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {

    def getLambdaFunName = "lambda$aaload_ref$"
    def getLambdaFunDesc = s"($vclasstype$fexprclasstype[$vclasstype)$vclasstype"
    def getInnerLambdaFunName = "lambda$aaload_idx$"
    def getInnerLambdaFunDesc = s"([${vclasstype}Ljava/lang/Integer;)$vclasstype"

    mv.visitInvokeDynamicInsn(
      "apply", // Method to invoke
      s"($vclasstype)Ljava/util/function/BiFunction;", // Descriptor for call site
      new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc), // Default LambdaFactory
      // Arguments:
      Type.getType("(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"),
      new Handle(H_INVOKESTATIC, env.clazz.name, getLambdaFunName, getLambdaFunDesc),
      Type.getType(s"($fexprclasstype[$vclasstype)$vclasstype")
    )
    loadCurrentCtx(mv, env, block)
    mv.visitMethodInsn(INVOKEINTERFACE,
      "edu/cmu/cs/varex/V",
      "sflatMap",
      s"(Ljava/util/function/BiFunction;$fexprclasstype)$vclasstype",
      true
    )

    if (!(env.clazz.lambdaMethods contains getLambdaFunName)) {
      val lambda = (cv: ClassVisitor) => {
        val mv: MethodVisitor = cv.visitMethod(
          ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
          getLambdaFunName,
          getLambdaFunDesc,
          getLambdaFunDesc, // TODO: signature here ignores the type inside V
          Array[String]() // TODO: handle exception list
        )
        mv.visitCode()
        mv.visitVarInsn(ALOAD, 0)   // index

        mv.visitVarInsn(ALOAD, 2)   // array ref
        mv.visitInvokeDynamicInsn(
          "apply",
          s"([$vclasstype)Ljava/util/function/Function;",
          new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc),
          // Arguments:
          Type.getType("(Ljava/lang/Object;)Ljava/lang/Object;"),
          new Handle(H_INVOKESTATIC, env.clazz.name, getInnerLambdaFunName, getInnerLambdaFunDesc),
          Type.getType(s"(Ljava/lang/Integer;)$vclasstype")
        )
        mv.visitVarInsn(ALOAD, 1) // load ctx
        mv.visitMethodInsn(INVOKEINTERFACE,
          "edu/cmu/cs/varex/V",
          "sflatMap",
          s"(Ljava/util/function/Function;$fexprclasstype)$vclasstype",
          true
        )
        mv.visitInsn(ARETURN)
        mv.visitMaxs(5, 5)
        mv.visitEnd()
      }
      env.clazz.lambdaMethods += (getLambdaFunName -> lambda)

      if (!(env.clazz.lambdaMethods contains getInnerLambdaFunName)) {
        val lambda = (cv: ClassVisitor) => {
          val mv: MethodVisitor = cv.visitMethod(
            ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
            getInnerLambdaFunName,
            getInnerLambdaFunDesc,
            getInnerLambdaFunDesc,
            Array[String]()
          )
          mv.visitCode()
          mv.visitVarInsn(ALOAD, 0) // array ref
          mv.visitVarInsn(ALOAD, 1) // index
          mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I", false)
          mv.visitInsn(AALOAD)
          mv.visitInsn(ARETURN)
          mv.visitMaxs(5, 5)
          mv.visitEnd()
        }
        env.clazz.lambdaMethods += (getInnerLambdaFunName -> lambda)
      }

    }
  }
}

/**
  * NEWARRAY is for primitive type. Since we are boxing all primitive types, all NEWARRAY should
  * be replaced by ANEWARRAY
  *
  * @todo
  *       We could keep primitive array, but loading and storing value from/to primitive array must be
  *       handled carefully because values outside array are all boxed.
  *
  * @todo
  *       By default, primitive array gets initialized after NEWARRAY, but now we are replacing it with ANEWARRAY,
  *       so we might need to initialize also
  */
case class InstrNEWARRAY(atype: Int) extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitIntInsn(NEWARRAY, atype)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      createVArray(mv, env, block)
    }
    else {
      mv.visitTypeInsn(ANEWARRAY, "edu/cmu/cs/varex/V")
      if (env.getTag(this, env.TAG_NEED_V)) {
        callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
      }
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (v, prev, f) = s.pop()
    if (env.getTag(this, env.TAG_NEED_V)) {
      // this means the array itself needs to be wrapped into a V
      (f.push(V_TYPE(), Set(this)), Set())
    }
    else {
      if (v == V_TYPE()) {
        // array length is a V, needs invokedynamic to create a V array ref
        env.setLift(this)
        (f.push(V_TYPE(), Set(this)), Set())
      }
      else {
        (f.push(REF_TYPE(), Set(this)), Set())
      }
    }
  }

  override def doBacktrack(env: VMethodEnv): Unit = env.setTag(this, env.TAG_NEED_V)
}

/**
  * Create new array of reference
  *
  * Operand Stack: ..., count -> ..., arrayref
  *
  * @param desc
  */
case class InstrANEWARRAY(desc: String) extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitTypeInsn(ANEWARRAY, desc)

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (v, prev, f) = s.pop()
    if (env.getTag(this, env.TAG_NEED_V)) {
      // this means the array itself needs to be wrapped into a V
      (f.push(V_TYPE(), Set(this)), Set())
    }
    else {
      if (v == V_TYPE()) {
        // array length is a V, needs invokedynamic to create a V array ref
        env.setLift(this)
        (f.push(V_TYPE(), Set(this)), Set())
      }
      else {
        (f.push(REF_TYPE(), Set(this)), Set())
      }
    }
  }

  /**
    * For ANEWARRAY, lifting means invokedynamic on a V array length
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      createVArray(mv, env, block)
    }
    else {
      mv.visitTypeInsn(ANEWARRAY, "edu/cmu/cs/varex/V")
      if (env.getTag(this, env.TAG_NEED_V)) {
        callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
      }
    }
  }

  override def doBacktrack(env: VMethodEnv): Unit = env.setTag(this, env.TAG_NEED_V)
}

/**
  * Store into reference array
  *
  * Operand Stack: ..., arrayref, index, value -> ...
  */
case class InstrAASTORE() extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(AASTORE)

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (vType, vPrev, frame1) = s.pop()
    val (idxType, idxPrev, frame2) = frame1.pop()
    val (refType, refPrev, frame3) = frame2.pop()
    // we assume that all elements in an array are of type V
    if (vType != V_TYPE()) return (frame3, vPrev)
    if (idxType != V_TYPE()) return (frame3, idxPrev)
    if (refType == V_TYPE()) {
      env.setLift(this)
    }
    (frame3, Set())
  }

  /**
    * For AASTORE, lifting means invokedynamic on a V object
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      storeOperation(mv, env, block)
    }
    else {
      mv.visitInsn(AASTORE)
    }
  }

  override def doBacktrack(env: VMethodEnv): Unit = {
    // do nothing, lifting or not depends on array ref type
  }
}

/**
  * Load reference from array
  *
  * Operand Stack: ..., arrayref, index -> ..., value
  */
case class InstrAALOAD() extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(AALOAD)

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (idxType, idxPrev, frame1) = s.pop()
    val (refType, refPrev, frame2) = frame1.pop()
    if (idxType != V_TYPE()) return (frame2, idxPrev)
    if (refType == V_TYPE()) {
      env.setLift(this)
    }
    (frame2.push(V_TYPE(), Set(this)), Set())
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadOperation(mv, env, block)
    }
    else {
      mv.visitInsn(AALOAD)
    }
  }

  override def doBacktrack(env: VMethodEnv): Unit = {
    // do nothing, lifting or not depends on ref type
  }
}

case class InstrIALOAD() extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(IALOAD)

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (idxType, idxPrev, frame1) = s.pop()
    val (refType, refPrev, frame2) = frame1.pop()
    if (idxType != V_TYPE()) return (frame2, idxPrev)
    if (refType == V_TYPE()) {
      env.setLift(this)
    }
    (frame2.push(V_TYPE(), Set(this)), Set())
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      loadOperation(mv, env, block)
    }
    else {
      mv.visitInsn(AALOAD)
    }
  }

  override def doBacktrack(env: VMethodEnv): Unit = {
    // do nothing, lifting or not depends on ref type
  }
}

case class InstrIASTORE() extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(IASTORE)

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (vType, vPrev, frame1) = s.pop()
    val (idxType, idxPrev, frame2) = frame1.pop()
    val (refType, refPrev, frame3) = frame2.pop()
    // we assume that all elements in an array are of type V
    if (vType != V_TYPE()) return (frame3, vPrev)
    if (idxType != V_TYPE()) return (frame3, idxPrev)
    if (refType == V_TYPE()) {
      env.setLift(this)
    }
    (frame3, Set())
  }

  /**
    * For IASTORE, lifting means invokedynamic on a V object
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      storeOperation(mv, env, block)
    }
    else {
      mv.visitInsn(AASTORE)
    }
  }

  override def doBacktrack(env: VMethodEnv): Unit = {
    // do nothing, lifting or not depends on array ref type
  }
}