package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{INT_TYPE, VBCFrame, VBCType, V_TYPE}
import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Type}

/**
  * @author chupanw
  */

/**
  * ICONST_n instruction
  *
  * @param v
  */
case class InstrICONST(v: Int) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    pushConstant(mv, v)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      pushConstant(mv, v)
      mv.visitMethodInsn(INVOKESTATIC, IntClass, "valueOf", s"(I)$IntType", false)
      callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
    }
    else {
      pushConstant(mv, v)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val newFrame =
      if (env.shouldLiftInstr(this))
        s.push(V_TYPE(), Set(this))
      else
        s.push(INT_TYPE(), Set(this))
    (newFrame, Set())
  }
}


case class InstrLDC(o: Object) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitLdcInsn(o)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, blockA: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      mv.visitLdcInsn(o)
      o match {
        case s: String => // do nothing
        case i: Integer => mv.visitMethodInsn(INVOKESTATIC, IntClass, "valueOf", s"(I)$IntType", false)
        case _ => throw new UnsupportedOperationException("Unsupported LDC type")
      }
      callVCreateOne(mv, (m) => loadCurrentCtx(m, env, blockA))
    }
    else {
      mv.visitLdcInsn(o)
      o match {
        case s: String => // do nothing
        case i: Integer => mv.visitMethodInsn(INVOKESTATIC, IntClass, "valueOf", s"(I)$IntType", false)
        case _ => throw new UnsupportedOperationException("Unsupported LDC type")
      }
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val newFrame =
      if (env.shouldLiftInstr(this))
        s.push(V_TYPE(), Set(this))
      else
        o match {
          case i: java.lang.Integer => s.push(INT_TYPE(), Set(this))
          case str: java.lang.String => s.push(VBCType(Type.getObjectType("java/lang/String")), Set(this))
          case _ => throw new RuntimeException("Incomplete support for LDC")
        }
    (newFrame, Set())
  }
}

case class InstrACONST_NULL() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = mv.visitInsn(ACONST_NULL)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      mv.visitInsn(ACONST_NULL)
      callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
    }
    else {
      mv.visitInsn(ACONST_NULL)
    }

  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    if (env.shouldLiftInstr(this))
      (s.push(V_TYPE(), Set(this)), Set())
    else
      (s.push(VBCType(Type.getObjectType("null")), Set(this)), Set())
  }
}

case class InstrLCONST(v: Int) extends Instruction {
  require(v == 0 || v == 1)

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    if (v == 0) mv.visitInsn(LCONST_0) else mv.visitInsn(LCONST_1)
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