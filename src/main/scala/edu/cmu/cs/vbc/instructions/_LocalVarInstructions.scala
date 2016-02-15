package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

/**
  * ISTORE instruction
  * @param variable
  */
case class InstrISTORE(variable: LocalVar) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitVarInsn(ISTORE, env.getVarIdx(variable))

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    //TODO is it worth optimizing this in case ctx is TRUE (or the initial method's ctx)?

    //new value is already on top of stack
    loadFExpr(mv, env, env.getBlockVar(block))
    mv.visitInsn(SWAP)
    loadV(mv, env, variable)
    //now ctx, newvalue, oldvalue on stack
    callVCreateChoice(mv)
    //now new choice value on stack combining old and new value
    storeV(mv, env, variable)
  }

  override def getVariables() = Set(variable)
}


/**
  * ILOAD instruction
  * @param variable
  */
case class InstrILOAD(variable: LocalVar) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitVarInsn(ILOAD, env.getVarIdx(variable))

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    loadV(mv, env, variable)
  }

  override def getVariables() = Set(variable)
}


/**
  * IINC instruction
  * @param variable
  * @param increment
  */
case class InstrIINC(variable: LocalVar, increment: Int) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitIincInsn(env.getVarIdx(variable), increment)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    loadV(mv, env, variable)
    pushConstant(mv, increment)
    mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IINC", "(Ledu/cmu/cs/varex/V;I)Ledu/cmu/cs/varex/V;", false)

    //create a choice with the original value
    loadFExpr(mv, env, env.getBlockVar(block))
    mv.visitInsn(SWAP)
    loadV(mv, env, variable)
    callVCreateChoice(mv)

    storeV(mv, env, variable)
  }

  override def getVariables() = Set(variable)
}


/**
  * ALOAD instruction
  */
case class InstrALOAD(variable: Int) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitVarInsn(ALOAD, variable)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitVarInsn(ALOAD, variable)
  }
}
