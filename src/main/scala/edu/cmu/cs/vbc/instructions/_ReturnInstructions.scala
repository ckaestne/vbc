package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

/**
  * RETURN instruction
  */
case class InstrRETURN() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitInsn(RETURN)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit =
    mv.visitInsn(RETURN)

  override def isReturnInstr: Boolean = true

}


case class InstrIRETURN() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(IRETURN)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitInsn(IRETURN)
  }

  override def isReturnInstr: Boolean = true
}
