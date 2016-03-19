package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.{VBCValue, VBCFrame}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.{Type, MethodVisitor}
import org.objectweb.asm.Opcodes._

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

    override def updateStack(s: VBCFrame) = s.push(VBCValue.newValue(Type.getObjectType(t)), this)
}
