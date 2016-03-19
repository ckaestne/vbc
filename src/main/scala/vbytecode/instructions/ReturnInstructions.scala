package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

trait ReturnInstruction extends Instruction {
    override def isReturnInstr: Boolean = true
}

/**
  * RETURN instruction
  */
case class InstrRETURN() extends ReturnInstruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
        mv.visitInsn(RETURN)

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit =
        mv.visitInsn(RETURN)
}


case class InstrIRETURN() extends ReturnInstruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitInsn(IRETURN)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        // Instead of returning an Integer, we return a reference
        mv.visitInsn(ARETURN)
    }
}


case class InstrARETURN() extends ReturnInstruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
        mv.visitInsn(ARETURN)

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit =
        mv.visitInsn(ARETURN)
}
