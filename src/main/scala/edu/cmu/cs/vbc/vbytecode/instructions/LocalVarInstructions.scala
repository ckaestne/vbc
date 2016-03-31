package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.{REF_TYPE, VBCFrame, V_TYPE}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

/**
  * ISTORE instruction
  *
  * @param variable
  */
case class InstrISTORE(variable: Variable) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitVarInsn(ISTORE, env.getVarIdx(variable))

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
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
    else
      mv.visitVarInsn(ISTORE, env.getVarIdx(variable))
  }

  override def getVariables() = {
    variable match {
      case p: Parameter => Set()
      case lv: LocalVar => Set(lv)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Option[Instruction]) = {
    // Now we assume all blocks are executed under some ctx other than method ctx,
    // meaning that all local variables should be a V, and so all ISTORE instructions
    // should be lifted
    env.setLift(this)
    val (value, prev, frame) = s.pop()
    val newFrame = frame.setLocal(variable, V_TYPE(), Some(this))
    val backtrack =
      if (value != V_TYPE())
        prev
      else
        None
    (newFrame, backtrack)
  }
}


/**
  * ILOAD instruction
  *
  * @param variable
  */
case class InstrILOAD(variable: Variable) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitVarInsn(ILOAD, env.getVarIdx(variable))

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this))
      loadV(mv, env, variable)
    else
      mv.visitVarInsn(ILOAD, env.getVarIdx(variable))
  }

  override def getVariables() = {
    variable match {
      case p: Parameter => Set()
      case lv: LocalVar => Set(lv)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Option[Instruction]) = {
    env.setLift(this)
    val newFrame = s.push(V_TYPE(), Some(this))
    val backtrack =
      if (s.localVar(variable)._1 != V_TYPE())
        s.localVar(variable)._2
      else
        None
    (newFrame, backtrack)
  }
}


/**
  * IINC instruction
  *
  * @param variable
  * @param increment
  */
case class InstrIINC(variable: Variable, increment: Int) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
    mv.visitIincInsn(env.getVarIdx(variable), increment)

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
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
    else
      toByteCode(mv, env, block)
  }

  override def getVariables() = {
    variable match {
      case p: Parameter => Set()
      case lv: LocalVar => Set(lv)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Option[Instruction]) = {
    // Now we assume all blocks are executed under some ctx other than method ctx,
    // meaning that all local variables should be a V, and so IINC instructions
    // should be lifted
    env.setLift(this)
    val newFrame = s.setLocal(variable, V_TYPE(), Some(this))
    (newFrame, None)
  }
}


/**
  * ALOAD instruction
  */
case class InstrALOAD(variable: Variable) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    val idx = env.getVarIdx(variable)
    mv.visitVarInsn(ALOAD, idx)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    /*
     * Behavior of ALOAD is the same no matter V or not V
     */
    val idx = env.getVarIdx(variable)
    mv.visitVarInsn(ALOAD, idx)
    if (env.shouldLiftInstr(this))
      callVCreateOne(mv)
  }

  override def getVariables() = {
    variable match {
      case p: Parameter => Set()
      case lv: LocalVar => Set(lv)
    }
  }

  /**
    * Used to identify the start of init method
    *
    * @see [[Rewrite.rewrite()]]
    */
  override def isALOAD0: Boolean = variable.getIdx().contains(0)

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Option[Instruction]) = {
    /*
     * This assumes that all local variables other than this parameter to be V.
     *
     * In the future, if STORE operations are optimized, this could also be optimized to avoid loading V and
     * save some instructions.
     */
    if (!env.shouldLiftInstr(this) && env.isL0(variable))
      (s.push(REF_TYPE(), Some(this)), None)
    else {
      val newFrame = s.push(V_TYPE(), Some(this))
      val backtrack =
        if (newFrame.localVar(variable)._1 != V_TYPE())
          newFrame.localVar(variable)._2
        else
          None
      (newFrame, backtrack)
    }
  }
}


/**
  * ASTORE: store reference into local variable
  *
  * @param variable
  */
case class InstrASTORE(variable: Variable) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    val idx = env.getVarIdx(variable)
    mv.visitVarInsn(ASTORE, idx)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this)) {
      /* new value is already on top of stack */
      loadFExpr(mv, env, env.getBlockVar(block))
      mv.visitInsn(SWAP)
      loadV(mv, env, variable)
      /* now ctx, newvalue, oldvalue on stack */
      callVCreateChoice(mv)
      /* now new choice value on stack combining old and new value */
      storeV(mv, env, variable)
    }
    else {
      val idx = env.getVarIdx(variable)
      mv.visitVarInsn(ASTORE, idx)
    }
  }

  override def getVariables = {
    variable match {
      case p: Parameter => Set()
      case lv: LocalVar => Set(lv)
    }
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Option[Instruction]) = {
    env.setLift(this)
    val (value, prev, frame) = s.pop()
    val newFrame = frame.setLocal(variable, V_TYPE(), Some(this))
    val backtrack =
      if (value != V_TYPE())
        prev
      else
        None
    (newFrame, backtrack)
  }
}

