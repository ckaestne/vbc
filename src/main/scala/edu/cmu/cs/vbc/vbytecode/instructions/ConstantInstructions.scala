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
        case s: String => mv.visitMethodInsn(INVOKESTATIC, StringClass, "valueOf", s"(Ljava/lang/String;)$StringType", false)
        case i: Integer => mv.visitMethodInsn(INVOKESTATIC, IntClass, "valueOf", s"(I)$IntType", false)
        case _ => throw new UnsupportedOperationException("Unsupported LDC type")
      }
      callVCreateOne(mv, (m) => loadCurrentCtx(m, env, blockA))
    }
    else {
      mv.visitLdcInsn(o)
      o match {
        case s: String => mv.visitMethodInsn(INVOKESTATIC, StringClass, "valueOf", s"(Ljava/lang/String;)$StringType", false)
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