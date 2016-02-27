package edu.cmu.cs.vbc.instructions

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
}


/**
  * Push byte
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
}


/**
  * SIPUSH: Push short
  * @param value
  */
case class InstrSIPUSH(value: Int) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitIntInsn(SIPUSH, value)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitIntInsn(SIPUSH, value)
    mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", getMtdDesc("I", JavaType("I")), false)
    callVCreateOne(mv)
  }
}
