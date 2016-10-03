package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{REF_TYPE, VBCFrame, V_TYPE}
import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.utils.{InvokeDynamicUtils, VCall}
import edu.cmu.cs.vbc.vbytecode.{Block, MethodEnv, Owner, VMethodEnv}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm._

/**
  * Our first attempt is to implement array as array of V. If array length is different, then arrayref itself
  * is also a V.
  */
trait ArrayInstructions extends Instruction {
  def createVArray(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    InvokeDynamicUtils.invoke(VCall.smap, mv, env, loadCurrentCtx(_, env, block), "anewarray", s"$IntType()[$vclasstype") {
      (visitor: MethodVisitor) => {
        visitor.visitVarInsn(ALOAD, 1)
        visitor.visitMethodInsn(INVOKEVIRTUAL, IntClass, "intValue", "()I", false) // JVM specification requires an int
        visitor.visitTypeInsn(ANEWARRAY, "edu/cmu/cs/varex/V")
        visitor.visitInsn(ARETURN)
      }
    }
  }

  def storeOperation(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    InvokeDynamicUtils.invoke(VCall.sforeach, mv, env, loadCurrentCtx(_, env, block), "aastore", s"[$vclasstype($IntType$vclasstype)V", nExplodeArgs = 1) {
      (visitor: MethodVisitor) => {
        visitor.visitVarInsn(ALOAD, 1) //array ref
        visitor.visitVarInsn(ALOAD, 3) //index
        visitor.visitMethodInsn(INVOKEVIRTUAL, IntClass, "intValue", "()I", false)
        visitor.visitVarInsn(ALOAD, 0) //new value
        visitor.visitInsn(AASTORE)
        visitor.visitInsn(RETURN)
      }
    }
  }

  def loadOperation(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    InvokeDynamicUtils.invoke(VCall.sflatMap, mv, env, loadCurrentCtx(_, env, block), "aaload", s"[$vclasstype($IntType)$vclasstype", nExplodeArgs = 1) {
      (visitor: MethodVisitor) => {
        visitor.visitVarInsn(ALOAD, 0) // array ref
        visitor.visitVarInsn(ALOAD, 2) // index
        visitor.visitMethodInsn(INVOKEVIRTUAL, IntClass, "intValue", "()I", false)
        visitor.visitInsn(AALOAD)
        visitor.visitInsn(ARETURN)
      }
    }
  }
}

/**
  * NEWARRAY is for primitive type. Since we are boxing all primitive types, all NEWARRAY should
  * be replaced by ANEWARRAY
  *
  * @todo We could keep primitive array, but loading and storing value from/to primitive array must be
  *       handled carefully because values outside array are all boxed.
  * @todo By default, primitive array gets initialized after NEWARRAY, but now we are replacing it with ANEWARRAY,
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
  * @param owner
  */
case class InstrANEWARRAY(owner: Owner) extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitTypeInsn(ANEWARRAY, owner)

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

/**
  * Store into char array
  *
  * Operand stack: ..., arrayref, index, value -> ...
  */
case class InstrCASTORE() extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(CASTORE)
  }

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
    * Lifting means performing operations on a V object
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
  * Load char from array
  *
  * Operand stack: ..., arrayref, index -> ..., value
  *
  * @todo Treating char array as V array lose the ability to print the char, sys.out could not
  *       tell whether this is a char or simply an integer
  */
case class InstrCALOAD() extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(CALOAD)
  }

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

/** Get length of array
  *
  * ..., arrayref -> ..., length
  */
case class InstrARRAYLENGTH() extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(ARRAYLENGTH)
  }

  /** Lifting means invoking on V arrayref */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    ???
//    if (env.shouldLiftInstr(this)) {
//      InvokeDynamicUtils.invoke("sflatMap", mv, env, )
//    }
//    else
//      mv.visitInsn(ARRAYLENGTH)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    ???
    val (vt, _, frame) = s.pop()
    if (vt == V_TYPE()) env.setLift(this)
    (frame, Set())
  }
}

case class InstrBALOAD() extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(BALOAD)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = ???

  /**
    * Update the stack symbolically after executing this instruction
    *
    * @return UpdatedFrame is a tuple consisting of new VBCFrame and a backtrack instructions.
    *         If backtrack instruction set is not empty, we need to backtrack because we finally realise we need to lift
    *         that instruction. By default every backtracked instruction should be lifted, except for GETFIELD,
    *         PUTFIELD, INVOKEVIRTUAL, and INVOKESPECIAL, because lifting them or not depends on the type of object
    *         currently on stack. If the object is a V, we need to lift these instructions with INVOKEDYNAMIC.
    *
    *         If backtrack instruction set is not empty, the returned VBCFrame is useless, current frame will be pushed
    *         to queue again and reanalyze later. (see [[edu.cmu.cs.vbc.analysis.VBCAnalyzer.computeBeforeFrames]]
    */
  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = ???
}

case class InstrBASTORE() extends ArrayInstructions {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(BASTORE)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = ???

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = ???
}