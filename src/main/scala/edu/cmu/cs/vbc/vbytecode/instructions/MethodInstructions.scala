package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.OpcodePrint
import edu.cmu.cs.vbc.analysis.VBCFrame.{FrameEntry, UpdatedFrame}
import edu.cmu.cs.vbc.analysis._
import edu.cmu.cs.vbc.model.LiftCall._
import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.utils.{InvokeDynamicUtils, LiftingFilter}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Type}

/**
  * @author chupanw
  */

trait MethodInstruction extends Instruction {

  def invokeDynamic(owner: String, name: String, desc: String, itf: Boolean, mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    val nArgs = Type.getArgumentTypes(desc).length
    val hasVArgs = nArgs > 0
    val (nOwner, nName, nDesc) = liftCall(hasVArgs, owner, name, desc)
    val objType = Type.getObjectType(nOwner).toString
    val argTypes = "(" + vclasstype * nArgs + ")"

    val isReturnVoid = Type.getReturnType(desc) == Type.VOID_TYPE
    val retType = if (isReturnVoid) "V" else vclasstype
    val vCall = if (isReturnVoid) "sforeach" else "sflatMap"

    val invokeType: Int = this match {
      case i: InstrINVOKEVIRTUAL => INVOKEVIRTUAL
      case i: InstrINVOKESPECIAL => INVOKESPECIAL
      case i: InstrINVOKEINTERFACE => INVOKEINTERFACE
      case _ => throw new UnsupportedOperationException("Unsupported invoke type")
    }

    InvokeDynamicUtils.invoke(vCall, mv, env, block, OpcodePrint.print(invokeType) + "$" + name, s"$objType$argTypes$retType") {
      (mv: MethodVisitor) => {
        mv.visitVarInsn(ALOAD, nArgs + 1) // objref
        0 until nArgs foreach { (i) => mv.visitVarInsn(ALOAD, i) } // arguments
        mv.visitVarInsn(ALOAD, nArgs) // ctx
        mv.visitMethodInsn(invokeType, nOwner, nName, nDesc, itf)
        if (!LiftingFilter.shouldLiftMethod(owner, name, desc) && !hasVArgs) callVCreateOne(mv, (m) => m.visitVarInsn(ALOAD, nArgs))
        if (isReturnVoid) mv.visitInsn(RETURN) else mv.visitInsn(ARETURN)
      }
    }
  }

  override def doBacktrack(env: VMethodEnv): Unit = env.setTag(this, env.TAG_NEED_V)

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
    val hasVArgs = argList.exists(_._1 == V_TYPE())

    // object reference
    var frame = s
    1 to nArg foreach { _ => frame = frame.pop()._3 }
    if (!this.isInstanceOf[InstrINVOKESTATIC]) {
      val (ref, ref_prev, baseFrame) = frame.pop() // L0
      frame = baseFrame
      if (ref == V_TYPE()) env.setLift(this)
      if (this.isInstanceOf[InstrINVOKESPECIAL] && name == "<init>") {
        if (ref.isInstanceOf[V_REF_TYPE]) {
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
        else {
          // ref is UNINITIALIZED_TYPE
          val (ref2, prev2, frame2) = frame.pop()
          assert(ref2 == ref, "Two different uninitialized refs on stack")
          if (!shouldLift && hasVArgs) {
            // we are going to use model class to do initialization
            frame = frame2.push(V_TYPE(), prev2)
          }
          else {
            frame = frame2.push(REF_TYPE(), prev2)
          }
        }
      }
    }

    // arguments
    if (hasVArgs) env.setTag(this, env.TAG_HAS_VARG)
    if (hasVArgs || shouldLift || env.shouldLiftInstr(this)) {
      // ensure that all arguments are V
      for (ele <- argList if ele._1 != V_TYPE()) return (s, ele._2)
    }

    // return value
    if (Type.getReturnType(desc) != Type.VOID_TYPE) {
      if (env.getTag(this, env.TAG_NEED_V))
        frame = frame.push(V_TYPE(), Set(this))
      else if (env.shouldLiftInstr(this))
        frame = frame.push(V_TYPE(), Set(this))
      else if (shouldLift || (hasVArgs && !shouldLift))
        frame = frame.push(V_TYPE(), Set(this))
      else
        frame = frame.push(VBCType(Type.getReturnType(desc)), Set(this))
    }

    // For exception handling, method invocation implies the end of a block
    //    val backtrack = backtraceNonVStackElements(frame)
    //    (frame, backtrack)
    (frame, Set())
  }

  def backtraceNonVStackElements(f: VBCFrame): Set[Instruction] = {
    (Tuple2[VBCType, Set[Instruction]](V_TYPE(), Set()) /: f.stack) (
      (a: FrameEntry, b: FrameEntry) => {
        // a is always V_TYPE()
        if (a._1 != b._1) (a._1, a._2 ++ b._2)
        else a
      })._2
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
      invokeDynamic(owner, name, desc, itf, mv, env, block)
    }
    else {
      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
      val (nOwner, nName, nDesc) = liftCall(hasVArgs, owner, name, desc)

      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESPECIAL, nOwner, nName, nDesc, itf)

      if (env.getTag(this, env.TAG_WRAP_DUPLICATE)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
      if (env.getTag(this, env.TAG_NEED_V)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
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
      invokeDynamic(owner, name, desc, itf, mv, env, block)
    }
    else {
      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
      val (nOwner, nName, nDesc) = liftCall(hasVArgs, owner, name, desc)

      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKEVIRTUAL, nOwner, nName, nDesc, itf)

      if (env.getTag(this, env.TAG_NEED_V)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
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
    val (nOwner, nName, nDesc) = liftCall(hasVArgs, owner, name, desc)

    loadCurrentCtx(mv, env, block)
    mv.visitMethodInsn(INVOKESTATIC, nOwner, nName, nDesc, itf)

    if (env.getTag(this, env.TAG_NEED_V)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
    updateStack(s, env, owner, name, desc)
}

case class InstrINVOKEINTERFACE(owner: String, name: String, desc: String, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKEINTERFACE, owner, name, desc, itf)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack(s, env, owner, name, desc)

  /**
    * Should be the same as INVOKEVIRTUAL
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      invokeDynamic(owner, name, desc, itf, mv, env, block)
    }
    else {
      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
      val (nOwner, nName, nDesc) = liftCall(hasVArgs, owner, name, desc)

      loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKEINTERFACE, nOwner, nName, nDesc, itf)

      if (env.getTag(this, env.TAG_NEED_V)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
    }
  }
}