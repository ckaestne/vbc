package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{VBCFrame, VBCType, V_REF_TYPE}
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

    /**
      * Update the stack symbolically after executing this instruction
      *
      * @return UpdatedFrame is a tuple consisting of new VBCFrame and a backtrack instruction.
      *         If backtrack instruction is not None, we need to backtrack because we finally realise we need to lift
      *         that instruction. By default every backtracked instruction should be lifted, except for GETFIELD,
      *         PUTFIELD, INVOKEVIRTUAL, and INVOKESPECIAL, because lifting them or not depends on the type of object
      *         currently on stack. If the object is a V, we need to lift these instructions with INVOKEDYNAMIC.
      * @note NEW instruction requires special attention. Internally, NEW instruction puts the UNINITIALIZED type on stack,
      *       which is not subtype of java.lang.Object, and thus we could not wrap it into a V right after NEW. The
      *       workaround is to introduce a special NEW_REF_VALUE type in our analysis. Whenever NEW_REF_VALUE type value
      *       is taken from stack, we wrap the stack value into a V from there, before using the value. Now, only PUTFIELD
      *       instruction could expect a NEW_REF_VALUE from stack.
      */
    override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
        if (env.shouldLiftInstr(this)) {
            // If the same new instructions are analyzed, ID always gets updated
            (s.push(V_REF_TYPE(VBCType.nextID), Set(this)), Set.empty[Instruction])
        }
        else
            (s.push(VBCType(Type.getObjectType(t)), Set(this)), Set.empty[Instruction])
    }
}
