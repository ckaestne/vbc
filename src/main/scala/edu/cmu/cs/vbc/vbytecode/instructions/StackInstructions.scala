package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{INT_TYPE, VBCFrame, V_TYPE}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._


/**
  * DUP instruction
  */
case class InstrDUP() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(DUP)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    //TODO, when applied to LONG, use the int one instead of the 2byte one
    mv.visitInsn(DUP)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (v, prev, frame1) = s.pop()
    val frame2 = frame1.push(v, prev)
    val frame3 = frame2.push(v, prev)
    (frame3, Set())
  }
}


/**
  * POP instruction
  */
case class InstrPOP() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(POP)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitInsn(POP)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (v, prev, newFrame) = s.pop()
    (newFrame, Set())
  }
}


/**
  * Push byte
  *
  * @param value
  */
case class InstrBIPUSH(value: Int) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitIntInsn(BIPUSH, value)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      pushConstant(mv, value)
      mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
      callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
    }
    else
      toByteCode(mv, env, block)
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


/**
  * SIPUSH: Push short
  *
  * @param value
  */
case class InstrSIPUSH(value: Int) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitIntInsn(SIPUSH, value)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      mv.visitIntInsn(SIPUSH, value)
      mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", genSign("I", primitiveToObjectType("I")), false)
      callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
    }
    else
      toByteCode(mv, env, block)
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
