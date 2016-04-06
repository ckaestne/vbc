package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{VBCFrame, VBCType, V_REF_TYPE, V_TYPE}
import edu.cmu.cs.vbc.model.LiftCall._
import edu.cmu.cs.vbc.utils.LiftUtils._
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
    val flatMapName = "sflatMap"
    val lamdaFactoryOwner = "java/lang/invoke/LambdaMetafactory"
    val lamdaFactoryMethod = "metafactory"
    val lamdaFactoryDesc = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"

    val n = env.clazz.lambdaMethods.size
    def getLambdaFunName = "lambda$" + env.method.name.replace('<', '$').replace('>', '$') + "$" + n
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
    loadCurrentCtx(mv, env, block)
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
      val hasVArgument = if (nArg > 0) true else false
      val (invokeModelClass, invokeLifted, nOwner, nName, nDesc) = liftCall(hasVArgument, owner, name, desc, false)
      if (invokeLifted)
        mv.visitVarInsn(ALOAD, nArg) // load ctx
      if (invokeModelClass) {
        mv.visitMethodInsn(INVOKESTATIC, nOwner, nName, nDesc, true)
      }
      else {
        if (isSpecial)
          mv.visitMethodInsn(INVOKESPECIAL, nOwner, nName, nDesc, itf)
        else
          mv.visitMethodInsn(INVOKEVIRTUAL, nOwner, nName, nDesc, itf)
      }
      if (isReturnVoid) {
        // in general, we should use foreach instead of flatMap for function calls with void return
        // types. But as this will change as soon as we have exceptions, for now, we return just
        // One(null)
        mv.visitInsn(ACONST_NULL) // this is because flatMap requires some return values
        callVCreateOne(mv, (m) => m.visitVarInsn(ALOAD, nArg))
        mv.visitInsn(ARETURN)
      } else {
        // if there is no V arguments, we would just call the method on the object, and thus we need
        // to wrap it into a V so that our invariant could hold
        if (!invokeLifted)
          callVCreateOne(mv, (m) => pushConstantTRUE(m))
        mv.visitInsn(ARETURN)
      }
      mv.visitMaxs(nArg + 2, nArg + 2)
      mv.visitEnd()
    }

    env.clazz.lambdaMethods += (getLambdaFunName -> lambda)
  }

  override def doBacktrack(env: VMethodEnv): Unit = env.setTag(this, env.TAG_NEED_V_RETURN)

  def updateStack(
                   s: VBCFrame,
                   env: VMethodEnv,
                   owner: String,
                   name: String,
                   desc: String
                 ): UpdatedFrame = {
    val shouldLift = LiftingFilter.shouldLiftMethod(owner, name, desc)
    val nArg = Type.getArgumentTypes(desc).length
    val argList: List[(VBCType, Set[Instruction])] = s.stack.take(nArg)

    // arguments
    val hasVArgs = argList.exists(_._1 == V_TYPE())
    if (hasVArgs) env.setTag(this, env.TAG_HAS_VARG)
    if (hasVArgs || shouldLift) {
      // ensure that all arguments are V
      for (ele <- argList if ele._1 != V_TYPE()) return (s, ele._2)
    }

    // object reference
    var frame = s
    1 to nArg foreach { _ => frame = frame.pop()._3 }
    if (!this.isInstanceOf[InstrINVOKESTATIC]) {
      val (ref, ref_prev, baseFrame) = frame.pop() // L0
      frame = baseFrame
      if (ref == V_TYPE()) env.setLift(this)
      if (this.isInstanceOf[InstrINVOKESPECIAL] && ref.isInstanceOf[V_REF_TYPE] && name == "<init>") {
        // Special handling for <init>:
        // Whenever we see a V_REF_TYPE reference, we know that the initialized object would be consumed later as
        // method arguments or field values, so we scan the stack and wrap it into a V
        env.setTag(this, env.TAG_WRAP_DUPLICATE)
        // we only expect one duplicate on stack, otherwise calling one createOne is not enough
        assert(frame.stack.head._1 == ref, "No duplicate UNINITIALIZED value on stack")
        val moreThanOne = frame.stack.tail.contains(ref)
        assert(!moreThanOne, "More than one UNINITIALIZED value on stack")
        val (ref2, prev2, frame2) = frame.pop()
        frame = frame2.push(V_TYPE(), prev2)
      }
    }

    // return value
    if (Type.getReturnType(desc) != Type.VOID_TYPE) {
      if (env.getTag(this, env.TAG_NEED_V_RETURN))
        frame = frame.push(V_TYPE(), Set(this))
      else if (env.shouldLiftInstr(this))
        frame = frame.push(V_TYPE(), Set(this))
      else if (shouldLift || (hasVArgs && !shouldLift))
        frame = frame.push(V_TYPE(), Set(this))
      else
        frame = frame.push(VBCType(Type.getReturnType(desc)), Set(this))
    }
    (frame, Set())
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
      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
      val (invokeStatic, invokeLifted, nOwner, nName, nDesc) = liftCall(hasVArgs, owner, name, desc, false)
      if (invokeLifted) {
        loadCurrentCtx(mv, env, block)
      }

      if (invokeStatic)
        mv.visitMethodInsn(INVOKESTATIC, nOwner, nName, nDesc, true)
      else
        mv.visitMethodInsn(INVOKESPECIAL, nOwner, nName, nDesc, itf)

      if (env.getTag(this, env.TAG_WRAP_DUPLICATE)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
      if (env.getTag(this, env.TAG_NEED_V_RETURN)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
    }
  }

  /**
    * Used to identify the start of init method
    *
    * @see [[Rewrite.rewrite()]]
    */
  override def isINVOKESPECIAL_OBJECT_INIT: Boolean =
    owner == "java/lang/Object" && name == "<init>" && desc == "()V" && !itf

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
    updateStack(s, env, owner, name, desc)
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
    * Generate variability-aware method invocation.
    *
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
      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
      val (invokeStatic, invokeLifted, nOwner, nName, nDesc) = liftCall(hasVArgs, owner, name, desc, false)
      if (invokeLifted) {
        loadCurrentCtx(mv, env, block)
      }

      if (invokeStatic)
        mv.visitMethodInsn(INVOKESTATIC, nOwner, nName, nDesc, true)
      else
        mv.visitMethodInsn(INVOKEVIRTUAL, nOwner, nName, nDesc, itf)

      if (env.getTag(this, env.TAG_NEED_V_RETURN)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
    updateStack(s, env, owner, name, desc)
}


/**
  * INVOKESTATIC
  *
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKESTATIC(owner: String, name: String, desc: String, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESTATIC, owner, name, desc, itf)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
    val (invokeStatic, invokeLifted, nOwner, nName, nDesc) = liftCall(hasVArgs, owner, name, desc, true)
    if (invokeLifted) {
      loadCurrentCtx(mv, env, block)
    }

    mv.visitMethodInsn(INVOKESTATIC, nOwner, nName, nDesc, itf)

    if (env.getTag(this, env.TAG_NEED_V_RETURN)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
    updateStack(s, env, owner, name, desc)
}