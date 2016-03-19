package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.{INT_TYPE, VBCFrame}
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

    override def updateStack(s: VBCFrame) = {
        val (vtype, _, newFrame) = s.pop()
        newFrame.push(vtype, this).push(vtype, this)
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

    override def updateStack(s: VBCFrame) = s.pop()._3
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
        pushConstant(mv, value)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        callVCreateOne(mv)
    }
    override def updateStack(s: VBCFrame) = s.push(INT_TYPE(), this)
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
        mv.visitIntInsn(SIPUSH, value)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", genSign("I", primitiveToObjectType("I")), false)
        callVCreateOne(mv)
    }

    override def updateStack(s: VBCFrame) = s.push(INT_TYPE(), this)
}
