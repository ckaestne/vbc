package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode._
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
        // Instead of returning an Integer, we return a reference
        mv.visitInsn(ARETURN)
    }

    override def isReturnInstr: Boolean = true
}


case class InstrARETURN() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
        mv.visitInsn(ARETURN)

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit =
        mv.visitInsn(ARETURN)

    override def isReturnInstr: Boolean = true
}
