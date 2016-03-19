package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.{VBCFrame, VBCType}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Type}

/**
  * @author chupanw
  */

case class InstrNEW(t: String) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitTypeInsn(NEW, t)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        mv.visitTypeInsn(NEW, t)
    }

    override def updateStack(s: VBCFrame) = s.push(VBCType(Type.getObjectType(t)), this)
}
