package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{VBCFrame, VBCType, V_TYPE}
import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.utils.LiftingFilter
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm._

/**
  * @author chupanw
  */

trait FieldInstruction extends Instruction {
  def getFieldFromV(mv: MethodVisitor, env: VMethodEnv, block: Block, owner: String, name: String, desc: String): Unit = {

    val VType = "Ledu/cmu/cs/varex/V;"
    val invokeName = "apply"
    val flatMapDesc = s"(Ljava/util/function/Function;$fexprclasstype)$VType"
    val flatMapOwner = "edu/cmu/cs/varex/V"
    val flatMapName = "sflatMap"

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
    loadCurrentCtx(mv, env, block)
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
      //by default, fields should never be "null", but this can happen when they are not initialized yet
      //to preserve our invariant that no V value shall ever be null, this will explicitly check for null
      //and wrap the null in a One if necessary.
      val label = new Label()
      mv.visitInsn(DUP)
      mv.visitJumpInsn(IFNONNULL, label)
      callVCreateOne(mv, (m) => pushConstantTRUE(m))
      mv.visitLabel(label)
      mv.visitInsn(ARETURN)
      mv.visitMaxs(5, 5)
      mv.visitEnd()
    }

    env.clazz.lambdaMethods += (getLambdaFunName -> lambda)
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
      if (LiftingFilter.shouldLiftField(owner, name, desc)) {
        // fields are lifted, the desc should be V
        mv.visitFieldInsn(GETSTATIC, owner, name, "Ledu/cmu/cs/varex/V;")
      }
      else {
        // fields are not lifted but we need a V, so we wrap it into a V
        mv.visitFieldInsn(GETSTATIC, owner, name, desc)
        callVCreateOne(mv, (m) => loadCurrentCtx(mv, env, block))
      }
    }
    else {
      mv.visitFieldInsn(GETSTATIC, owner, name, desc)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    if (LiftingFilter.shouldLiftField(owner, name, desc)) {
      // This field should be lifted (e.g. fields that are not from java.lang)
      env.setLift(this)
      (s.push(V_TYPE(), Set(this)), Set())
    }
    else {
      if (env.shouldLiftInstr(this)) {
        (s.push(V_TYPE(), Set(this)), Set())
      }
      else {
        (s.push(VBCType(Type.getType(desc)), Set(this)), Set())
      }
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
    if (env.isConditionalField(owner, name, desc)) {
      mv.visitInsn(POP)
    }
    else {
      if (env.shouldLiftInstr(this))
        mv.visitFieldInsn(PUTSTATIC, owner, name, "Ledu/cmu/cs/varex/V;")
      else
        mv.visitFieldInsn(PUTSTATIC, owner, name, desc)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (v, prev, newFrame) = s.pop()
    env.setLift(this)
    val backtrack =
      if (v != V_TYPE())
        prev
      else
        Set[Instruction]()
    (newFrame, backtrack)
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
      getFieldFromV(mv, env, block, owner, name, desc)
    else
      mv.visitFieldInsn(GETFIELD, owner, name, "Ledu/cmu/cs/varex/V;")

    //select V to current context
    loadCurrentCtx(mv, env, block)
    mv.visitMethodInsn(INVOKEINTERFACE, vclassname, "select", s"($fexprclasstype)$vclasstype", true)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (v, prev, frame) = s.pop()
    if (v == V_TYPE()) env.setLift(this)
    val newFrame = frame.push(V_TYPE(), Set(this))
    (newFrame, Set())
  }

  override def doBacktrack(env: VMethodEnv): Unit = {
    // if backtracked, do nothing
    // lifting or not will be set in updateStack()
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
      mv.visitInsn(POP2)
    }
    else {
      /* At this point, stack should contain object reference and new value.
       * the object reference is variational if `env.shouldLiftInstr(this)` */

      // put operation is what we perform on a nonvariational `this` and
      // a variational value on the stack;
      // if `this` is variational, we execute this operation on every this using `sforeach`
      val putOperation = (mv: MethodVisitor, loadContext: (MethodVisitor) => Unit) => {
        //stack: ..., this, val

        /* get the old value (all configurations, no select!) */
        mv.visitInsn(SWAP) // stack: ..., val, this
        mv.visitInsn(DUP_X1) // stack: .., this, val, this
        mv.visitFieldInsn(GETFIELD, owner, name, "Ledu/cmu/cs/varex/V;") // stack: ..., this, val, oldval OR null

        //by default, fields should never be "null", but this can happen when they are not initialized yet
        //to preserve our invariant that no V value shall ever be null, this will explicitly check for null
        //and wrap the null in a One if necessary.
        val label = new Label()
        mv.visitInsn(DUP)
        mv.visitJumpInsn(IFNONNULL, label) // this is a nonvariational jump within this block, which does not interfere with out control flow strategyy
        callVCreateOne(mv, (m) => pushConstantTRUE(m))
        mv.visitLabel(label)

        /* put FE, new value and old value */
        loadContext(mv) // stack: ..., this, val, oldval, ctx
        mv.visitInsn(DUP_X2) // stack: ..., this, ctx, val, oldval, ctx
        mv.visitInsn(POP) // stack: ..., this, ctx, val, oldval
        callVCreateChoice(mv) // stack: ..., this, newval

        //        mv.visitInsn(DUP)
        //        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
        //        mv.visitLdcInsn(name+": ")
        //        mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "print", "(Ljava/lang/Object;)V", false)
        //        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
        //        mv.visitInsn(SWAP)
        //        mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V", false)

        // finally put the new variational value (`this` is not variational here)
        mv.visitFieldInsn(PUTFIELD, owner, name, "Ledu/cmu/cs/varex/V;") // stack: ...

      }


      val loadContext = (m: MethodVisitor) => loadCurrentCtx(m, env, block)

      if (env.shouldLiftInstr(this)) {
        callPutOnV(mv, env, loadContext, putOperation)
      }
      else {
        putOperation(mv, loadContext)
      }
    }
  }

  /**
    * implementation to execute the put operation on a variational `this`. This means
    * that we use sforeach in the current context to execute the put operation on
    * each `this`, propagating the correct context to the inner children
    */
  private def callPutOnV(mv: MethodVisitor,
                         env: VMethodEnv,
                         loadCurrentCtx: (MethodVisitor) => Unit,
                         putOperation: (MethodVisitor, (MethodVisitor) => Unit) => Unit): Unit = {
    val invokeName = "accept"
    val flatMapDesc = s"(Ljava/util/function/BiConsumer;$fexprclasstype)V"
    val flatMapOwner = "edu/cmu/cs/varex/V"
    val flatMapName = "sforeach"

    val n = env.clazz.lambdaMethods.size
    def getLambdaFunName = "lambda$PUT" + owner.replace("/", "$") + "$" + name
    def getLambdaFunDesc = s"($vclasstype$fexprclasstype${Type.getObjectType(owner)})V"
    def getInvokeType = s"($vclasstype)Ljava/util/function/BiConsumer;"


    mv.visitInvokeDynamicInsn(
      invokeName, // Method to invoke
      getInvokeType, // Descriptor for call site
      new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc), // Default LambdaFactory
      // Arguments:
      Type.getType(s"(Ljava/lang/Object;Ljava/lang/Object;)V"),
      new Handle(H_INVOKESTATIC, env.clazz.name, getLambdaFunName, getLambdaFunDesc),
      Type.getType(s"($fexprclasstype${Type.getObjectType(owner)})V")
    )
    loadCurrentCtx(mv)
    mv.visitMethodInsn(INVOKEINTERFACE, flatMapOwner, flatMapName, flatMapDesc, true)

    /**
      * this function should be unique for an owner-name combination. create only once
      *
      * TODO: actually, this method should probably be in the class that contains the
      * field, not redundantly in every class that accesses this field
      */
    if (!(env.clazz.lambdaMethods contains getLambdaFunName)) {
      val lambda = (cv: ClassVisitor) => {
        val mv: MethodVisitor = cv.visitMethod(
          ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
          getLambdaFunName,
          getLambdaFunDesc,
          getLambdaFunDesc,
          Array[String]() // TODO: handle exception list
        )
        mv.visitCode()
        mv.visitVarInsn(ALOAD, 2) // load obj
        mv.visitVarInsn(ALOAD, 0)
        val loadContext = (mv: MethodVisitor) => mv.visitVarInsn(ALOAD, 1)
        putOperation(mv, loadContext)
        mv.visitInsn(RETURN)
        mv.visitMaxs(5, 5)
        mv.visitEnd()
      }

      env.clazz.lambdaMethods += (getLambdaFunName -> lambda)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (value, prev1, frame) = s.pop()
    val (ref, prev2, newFrame) = frame.pop()
    if (value != V_TYPE()) return (newFrame, prev1)
    if (ref == V_TYPE()) env.setLift(this)
    (newFrame, Set())
  }

  override def doBacktrack(env: VMethodEnv): Unit = {
    // if backtracked, do nothing
    // lifting or not will be set in updateStack()
  }
}