package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame
import edu.cmu.cs.vbc.vbytecode.{Block, MethodEnv, VMethodEnv}
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

case class InstrMONITORENTER() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(MONITORENTER)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    System.err.println("WARNING: MONITOR is used in: " + env.clazz.name + "#" + env.method.name)
    mv.visitInsn(MONITORENTER)
  }

  /**
    * Update the stack symbolically after executing this instruction
    *
    * @return UpdatedFrame is a tuple consisting of new VBCFrame and a backtrack instructions.
    *         If backtrack instruction set is not empty, we need to backtrack because we finally realise we need to lift
    *         that instruction. By default every backtracked instruction should be lifted, except for GETFIELD,
    *         PUTFIELD, INVOKEVIRTUAL, and INVOKESPECIAL, because lifting them or not depends on the type of object
    *         currently on stack. If the object is a V, we need to lift these instructions with INVOKEDYNAMIC.
    *
    *         If backtrack instruction set is not empty, the returned VBCFrame is useless, current frame will be pushed
    *         to queue again and reanalyze later. (see [[edu.cmu.cs.vbc.analysis.VBCAnalyzer.computeBeforeFrames]]
    */
  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (_, _, frame) = s.pop()
    (frame, Set())
  }
}

case class InstrMONITOREXIT() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(MONITOREXIT)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitInsn(MONITOREXIT)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    val (_, _, frame) = s.pop()
    (frame, Set())
  }
}