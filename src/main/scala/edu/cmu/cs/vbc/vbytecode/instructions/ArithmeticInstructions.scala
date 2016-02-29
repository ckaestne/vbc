package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

/**
  * IADD instruction
  */
case class InstrIADD() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitInsn(IADD)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IADD", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
    }
}


case class InstrISUB() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitInsn(ISUB)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "ISUB", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
    }
}


case class InstrIMUL() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitInsn(IMUL)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IMUL", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
    }
}


case class InstrIDIV() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitInsn(IDIV)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IDIV", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
    }
}