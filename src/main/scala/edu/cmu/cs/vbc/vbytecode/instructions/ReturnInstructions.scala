package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{VBCFrame, V_TYPE}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

sealed trait ReturnInstruction extends Instruction {
  override def isReturnInstr: Boolean = true
}

/**
  * RETURN instruction
  */
case class InstrRETURNVoid() extends ReturnInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitInsn(RETURN)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit =
    mv.visitInsn(RETURN)

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = (s, Set())
}


case class InstrRETURNVal(opcode: Int) extends ReturnInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(opcode)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    // Instead of returning an Integer or whatever plain type, we return a V reference
    mv.visitInsn(ARETURN)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    env.setLift(this)
    val (v, prev, newFrame) = s.pop()
    val backtrack =
      if (v != V_TYPE()) prev
      else Set[Instruction]()
    (newFrame, backtrack)
  }
}


/**
  * ATHROW has unusual characteristics. Originally it behaves like a return instruction,
  * but when lifted, it behaves like an unconditional jump. It is rewritten before
  * toVByteCode is called; the rewritten bytecode will no longer have any ATHROW instructions
  * but may have VInstrSTORE_Exception, JUMP, and VInstrRETURN instructions.
  *
  * TODO: on an unconditional path, we do not need to lift ATHROW operations
  */
case class InstrATHROW() extends ReturnInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(ATHROW)
  }

  // TODO support ATHROW in variational setting if we can guarantee it's not called
  // on a variational path.
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit =
    throw new UnsupportedOperationException("ATHROW cannot be used in variational context; should have been rewritten to InstrGOTO + VInstrRETURN")

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    //TODO: assume that we can pop an instance of Throwable (not V<Throwable>!)
    val (v, prev, newFrame) = s.pop()
    (newFrame, Set())
  }
}
