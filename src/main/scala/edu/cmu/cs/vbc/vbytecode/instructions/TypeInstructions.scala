package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{VBCFrame, VBCType, V_REF_TYPE, V_TYPE}
import edu.cmu.cs.vbc.utils.InvokeDynamicUtils
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
      (s.push(V_REF_TYPE(VBCType.nextID), Set(this)), Set())
    }
    else
      (s.push(VBCType(Type.getObjectType(t)), Set(this)), Set())
  }
}

/**
  * Check whether object is of given type
  *
  * If objref is null, stack unchanged (strange)
  * If objref is not null,
  *   unchanged if checkcast succeeds
  *   ClassCastException if checkcast fails
  *
  * Operand stack: ..., objref -> ..., objref
  */
case class InstrCHECKCAST(desc: String) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitTypeInsn(CHECKCAST, desc)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = {
    // TODO: since we don't support exception now, stack is unchanged
    if (s.stack.head._1 == V_TYPE()) {
      env.setLift(this)
    }
    (s, Set())
  }

  /**
    * Lifting means doing invokedynamic on a V object
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      InvokeDynamicUtils.invoke("smap", mv, env, block, "checkcast", "Ljava/lang/Object;()Ljava/lang/Object;") {
        (visitor: MethodVisitor) => {
          visitor.visitVarInsn(ALOAD, 1)  // ref
          visitor.visitTypeInsn(CHECKCAST, desc)
          visitor.visitInsn(ARETURN)
        }
      }
    }
    else {
      toByteCode(mv, env, block)
    }
  }
}


case class InstrI2C() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInsn(I2C)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = ???

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = ???
}
