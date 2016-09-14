package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.OpcodePrint
import edu.cmu.cs.vbc.analysis.VBCFrame.{FrameEntry, UpdatedFrame}
import edu.cmu.cs.vbc.analysis._
import edu.cmu.cs.vbc.model.LiftCall
import edu.cmu.cs.vbc.model.LiftCall._
import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.utils.{InvokeDynamicUtils, LiftingPolicy}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassVisitor, MethodVisitor, Type}

/**
  * @author chupanw
  */

trait MethodInstruction extends Instruction {

  def invokeDynamic(
                     owner: Owner,
                     name: MethodName,
                     desc: MethodDesc,
                     itf: Boolean,
                     mv: MethodVisitor,
                     env: VMethodEnv,
                     defaultLoadCtx: MethodVisitor => Unit
                   ): Unit = {
    val nArgs = Type.getArgumentTypes(desc).length
    val hasVArgs = nArgs > 0
    val liftedCall = liftCall(owner, name, desc)
    val objType = Type.getObjectType(liftedCall.owner).toString
    val argTypes: String = liftedCall.desc.getArgs.map(_.toObject).map(_.desc).mkString("(", "", ")")

    val isReturnVoid = Type.getReturnType(desc) == Type.VOID_TYPE
    val retType = if (isReturnVoid) "V" else vclasstype
    val vCall = if (isReturnVoid) "sforeach" else "sflatMap"

    val invokeType: Int = this match {
      case i: InstrINVOKEVIRTUAL => INVOKEVIRTUAL
      case i: InstrINVOKESPECIAL => INVOKESPECIAL
      case i: InstrINVOKEINTERFACE => INVOKEINTERFACE
      case _ => throw new UnsupportedOperationException("Unsupported invoke type")
    }

    InvokeDynamicUtils.invoke(
      vCall,
      mv,
      env,
      defaultLoadCtx,
      OpcodePrint.print(invokeType) + "$" + name.name,
      s"$objType$argTypes$retType",
      nExplodeArgs = liftedCall.desc.getArgCount
    ) {
      (mv: MethodVisitor) => {
        if (hasVArgs) {
          mv.visitVarInsn(ALOAD, 0) // objref
          1 until nArgs foreach { (i) => loadVar(i, liftedCall.desc, i - 1, mv) } // first nArgs -1 arguments
          loadVar(nArgs + 1, liftedCall.desc, nArgs - 1, mv) // last argument
        } else {
          mv.visitVarInsn(ALOAD, nArgs + 1) // objref
          0 until nArgs foreach { (i) => loadVar(i, liftedCall.desc, i, mv) } // arguments
        }
        if (liftedCall.isLifting) mv.visitVarInsn(ALOAD, nArgs) // ctx
        mv.visitMethodInsn(invokeType, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
        // Box primitive type
        boxReturnValue(liftedCall.desc, mv)
        if (!LiftingPolicy.shouldLiftMethodCall(owner, name, desc) && !isReturnVoid)
          callVCreateOne(mv, (m) => m.visitVarInsn(ALOAD, nArgs))
        if (isReturnVoid) mv.visitInsn(RETURN) else mv.visitInsn(ARETURN)
      }
    }
  }

  def boxReturnValue(desc: MethodDesc, mv: MethodVisitor): Unit = {
    desc.getReturnTypeSort match {
      case Type.INT =>
        mv.visitMethodInsn(INVOKESTATIC, Owner("java/lang/Integer"), MethodName("valueOf"), MethodDesc("(I)Ljava/lang/Integer;"), false)
      case Type.OBJECT => // do nothing
      case Type.VOID => // do nothing
      case _ => ???
    }
  }

  def loadVar(index: Int, desc: MethodDesc, indexInDesc: Int, mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, index)
    val args = desc.getArgs
    args(indexInDesc) match {
      case TypeDesc("Z") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getBoolean, MethodName("booleanValue"), MethodDesc("()Z"), false)
      case TypeDesc("C") => ???
      case TypeDesc("B") => ???
      case TypeDesc("S") => ???
      case TypeDesc("I") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getInt, MethodName("intValue"), MethodDesc("()I"), false)
      case TypeDesc("F") => ???
      case TypeDesc("J") => ???
      case TypeDesc("D") => ???
      case _ => // nothing
    }
  }

  /** Invoke methods on nonV object with V arguments
    *
    * [[edu.cmu.cs.vbc.analysis.VBCAnalyzer]] ensures that all V arguments will be Vs.
    * Call a helper static method instead of making the original call. Inside the helper method,
    * argument order will be tweaked so that we can call [[InvokeDynamicUtils.invoke()]]
    */
  def invokeOnNonV(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean, mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    val helperName = "helper$invokeOnNonV$" + env.clazz.lambdaMethods.size
    val helperDesc: String =
      "(" + owner.getTypeDesc.desc + vclasstype * desc.getArgCount + fexprclasstype + ")" +
        (if (desc.isReturnVoid) "V" else vclasstype)
    val helper = (cv: ClassVisitor) => {
      val m: MethodVisitor = cv.visitMethod(
        ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
        helperName, // method name
        helperDesc, // descriptor
        helperDesc, // signature
        Array[String]() // exception
      )
      m.visitCode()
      m.visitVarInsn(ALOAD, 0) // objref
      callVCreateOne(m, (m) => m.visitVarInsn(ALOAD, desc.getArgCount + 1))
      1 to desc.getArgCount foreach { (i) => m.visitVarInsn(ALOAD, i) } // arguments
      invokeDynamic(owner, name, desc, itf, m, env, defaultLoadCtx = (m) => m.visitVarInsn(ALOAD, desc.getArgCount + 1))
      if (desc.isReturnVoid) m.visitInsn(RETURN) else m.visitInsn(ARETURN)
      m.visitMaxs(10, 10)
      m.visitEnd()
    }
    env.clazz.lambdaMethods += (helperName -> helper)
    mv.visitMethodInsn(INVOKESTATIC, Owner(env.clazz.name), MethodName(helperName), MethodDesc(helperDesc), false)
  }

  override def doBacktrack(env: VMethodEnv): Unit = env.setTag(this, env.TAG_NEED_V)

  def updateStack(
                   s: VBCFrame,
                   env: VMethodEnv,
                   owner: Owner,
                   name: MethodName,
                   desc: MethodDesc
                 ): UpdatedFrame = {
    val shouldLift = LiftingPolicy.shouldLiftMethodCall(owner, name, desc)
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
      if (this.isInstanceOf[InstrINVOKESPECIAL] && name.contentEquals("<init>")) {
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
            // we are going to use special model class to do initialization
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
case class InstrINVOKESPECIAL(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean) extends MethodInstruction {
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
      invokeDynamic(owner, name, desc, itf, mv, env, loadCurrentCtx(_, env, block))
    }
    else {
      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
      val liftedCall = liftCall(owner, name, desc)

      if (liftedCall.isLifting) loadCurrentCtx(mv, env, block)
      if (name.contentEquals("<init>") && hasVArgs && !LiftingPolicy.shouldLiftMethodCall(owner, name, desc)) {
        // Use a special init method to do initialization
        val newDesc = liftedCall.desc.substring(0, liftedCall.desc.length - 1) + vclasstype
        val newName = LiftCall.encodeTypeInName(MethodName("Vinit"), desc)
        mv.visitMethodInsn(INVOKESTATIC, liftedCall.owner, newName, newDesc, false)
        // pop the useless uninitialized references
        mv.visitInsn(SWAP)
        mv.visitInsn(POP)
        mv.visitInsn(SWAP)
        mv.visitInsn(POP)
      }
      else {
        mv.visitMethodInsn(INVOKESPECIAL, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
      }

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
  owner.contentEquals("java/lang/Object") && name.contentEquals("<init>") && desc.contentEquals("()V") && !itf

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
case class InstrINVOKEVIRTUAL(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean) extends MethodInstruction {
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
      invokeDynamic(owner, name, desc, itf, mv, env, loadCurrentCtx(_, env, block))
    }
    else {
      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
      val liftedCall = liftCall(owner, name, desc)

      if (!liftedCall.isLifting && hasVArgs) {
        loadCurrentCtx(mv, env, block)
        invokeOnNonV(owner, name, desc, itf, mv, env, block)
      }
      else {
        if (liftedCall.isLifting) loadCurrentCtx(mv, env, block)
        mv.visitMethodInsn(INVOKEVIRTUAL, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
        if (env.getTag(this, env.TAG_NEED_V)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
      }
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
case class InstrINVOKESTATIC(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESTATIC, owner, name, desc, itf)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
    val liftedCall = liftCall(owner, name, desc)

    if (!liftedCall.isLifting && hasVArgs) {
      val vCall = if (liftedCall.desc.isReturnVoid) "sforeach" else "sflatMap"
      val lambdaName = "helper$invokestaticWithVs$" + env.clazz.lambdaMethods.size
      val args = liftedCall.desc.getArgs.map(_.toObject)
      val invokeDesc = args.head.desc + s"(${args.tail.map(_.desc).mkString("")})" + (if (liftedCall.desc.isReturnVoid) "V" else vclasstype)
      InvokeDynamicUtils.invoke(vCall, mv, env, loadCurrentCtx(_, env, block), lambdaName, invokeDesc, nExplodeArgs = args.size - 1) {
        (m: MethodVisitor) => {
          0 to args.length - 2 foreach { i => loadVar(i, liftedCall.desc, i, m) } // first args.size - 1 arguments
          loadVar(args.size, liftedCall.desc, args.size - 1, m) // last argument
          m.visitMethodInsn(INVOKESTATIC, liftedCall.owner, liftedCall.name, liftedCall.desc, false)
          boxReturnValue(liftedCall.desc, m)
          if (liftedCall.desc.isReturnVoid) {
            m.visitInsn(RETURN)
          }
          else {
            callVCreateOne(m, mm => mm.visitVarInsn(ALOAD, args.size - 1))
            m.visitInsn(ARETURN)
          }
        }
      }
    } else {
      if (liftedCall.isLifting) loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKESTATIC, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
      if (env.getTag(this, env.TAG_NEED_V)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
    updateStack(s, env, owner, name, desc)
}

case class InstrINVOKEINTERFACE(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKEINTERFACE, owner, name, desc, itf)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack(s, env, owner, name, desc)

  /**
    * Should be the same as INVOKEVIRTUAL
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      invokeDynamic(owner, name, desc, itf, mv, env, loadCurrentCtx(_, env, block))
    }
    else {
      val liftedCall = liftCall(owner, name, desc)

      if (liftedCall.isLifting) loadCurrentCtx(mv, env, block)
      mv.visitMethodInsn(INVOKEINTERFACE, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)

      if (env.getTag(this, env.TAG_NEED_V)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
    }
  }
}