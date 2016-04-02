package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.analysis.VBCFrame.{FrameEntry, UpdatedFrame}
import edu.cmu.cs.vbc.analysis.{VBCFrame, VBCType, V_TYPE}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

/**
  * IF* and GOTO instructions
  */
trait JumpInstruction extends Instruction {
  /**
    * gets the successor of a jump. the first value is the
    * target of an unconditional jump, but it can be None
    * when it just falls through to the next block
    * the second value is the target of a conditional jump,
    * if any
    */
  def getSuccessor(): (Option[Int], Option[Int])

  override def getJumpInstr: Option[JumpInstruction] = Some(this)

  def updateStack1(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (v1, prev1, newFrame) = s.pop()
    env.setLift(this)
    if (v1 != V_TYPE()) return (newFrame, prev1)
    val backtrack = backtraceNonVStackElements(s)
    (newFrame, backtrack)
  }

  def updateStack2(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    val (v1, prev1, frame1) = s.pop()
    val (v2, prev2, newFrame) = frame1.pop()
    env.setLift(this)
    if (v1 != V_TYPE()) return (newFrame, prev1)
    if (v1 != V_TYPE()) return (newFrame, prev2)
    val backtrack = backtraceNonVStackElements(s)
    (newFrame, backtrack)
  }

  def backtraceNonVStackElements(f: VBCFrame): Set[Instruction] = {
    (Tuple2[VBCType, Set[Instruction]](V_TYPE(), Set.empty[Instruction]) /: f.stack) (
      (a: FrameEntry, b: FrameEntry) => {
        // a is always V_TYPE()
        if (a._1 != b._1) (a._1, a._2 ++ b._2)
        else a
      })._2
  }
}

/**
  * assumptions (for now)
  *
  * the if statement is the last statement in a block, making a decision
  * between the next block or the referenced block
  *
  * for now, jumps can only be made forward, not backward (loops not yet
  * supported)
  *
  * for now, blocks need to be balanced wrt to the stack (not enforced yet)
  */
case class InstrIFEQ(targetBlockIdx: Int) extends JumpInstruction {


  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    val targetBlock = env.getBlock(targetBlockIdx)
    mv.visitJumpInsn(IFEQ, env.getBlockLabel(targetBlock))
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {

    /**
      * creating a variable for the decision
      *
      * on top of the stack is the condition, which should be V[Int];
      * that is, we want to know when that value is different from 0
      *
      * the condition is then stored as feature expression in a new
      * variable. this variable is used at the beginning of the relevant
      * blocks to modify the ctx
      *
      * the actual modification of ctx happens Block.toVByteCode
      */

    if (env.shouldLiftInstr(this))
      mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenEQ", "(Ledu/cmu/cs/varex/V;)Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
    //only evaluate condition, jump in block implementation
    else
      mv.visitJumpInsn(IFEQ, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def getSuccessor() = (None, Some(targetBlockIdx))

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack1(s, env)
}


/**
  * InstrIFNE: jump if the value on top of stack is not 0
  *
  * @param targetBlockIdx
  */
case class InstrIFNE(targetBlockIdx: Int) extends JumpInstruction {

  override def getSuccessor(): (Option[Int], Option[Int]) = (None, Some(targetBlockIdx))

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitJumpInsn(IFNE, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this))
      mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenNE", "(Ledu/cmu/cs/varex/V;)Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
    else
      mv.visitJumpInsn(IFNE, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack1(s, env)
}


case class InstrGOTO(targetBlockIdx: Int) extends JumpInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    val targetBlock = env.getBlock(targetBlockIdx)
    mv.visitJumpInsn(GOTO, env.getBlockLabel(targetBlock))
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (!env.shouldLiftInstr(this)) {
      mv.visitJumpInsn(GOTO, env.getBlockLabel(env.getBlock(targetBlockIdx)))
    }
    // if we should lift GOTO, it will be handled in block implementation
  }


  override def getSuccessor() = (Some(targetBlockIdx), None)

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    env.setLift(this)
    val backtrack = backtraceNonVStackElements(s)
    (s, backtrack)
  }
}


case class InstrIF_ICMPEQ(targetBlockIdx: Int) extends JumpInstruction {
  /**
    * (Unconditional target, Conditional target)
    * None if next block for unconditional target
    */
  override def getSuccessor(): (Option[Int], Option[Int]) = (None, Some(targetBlockIdx))

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitJumpInsn(IF_ICMPEQ, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this))
      mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenIEQ", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
    else
      mv.visitJumpInsn(IF_ICMPEQ, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack2(s, env)
}


case class InstrIF_ICMPGE(targetBlockIdx: Int) extends JumpInstruction {
  /**
    * (Unconditional target, Conditional target)
    * None if next block for unconditional target
    */
  override def getSuccessor(): (Option[Int], Option[Int]) = (None, Some(targetBlockIdx))

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitJumpInsn(IF_ICMPGE, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this))
      mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenIGE", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
    else
      mv.visitJumpInsn(IF_ICMPGE, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack2(s, env)
}


case class InstrIFGE(targetBlockIdx: Int) extends JumpInstruction {
  /**
    * (Unconditional target, Conditional target)
    * None if next block for unconditional target
    */
  override def getSuccessor(): (Option[Int], Option[Int]) = (None, Some(targetBlockIdx))

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitJumpInsn(IFGE, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this))
      mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenGE", genSign(vclasstype, fexprclasstype), false)
    else
      mv.visitJumpInsn(IFGE, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack1(s, env)
}


case class InstrIF_ICMPLT(targetBlockIdx: Int) extends JumpInstruction {
  /**
    * (Unconditional target, Conditional target)
    * None if next block for unconditional target
    */
  override def getSuccessor(): (Option[Int], Option[Int]) = (None, Some(targetBlockIdx))

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitJumpInsn(IF_ICMPLT, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this))
      mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenILT", genSign(vclasstype, vclasstype, fexprclasstype), false)
    else
      mv.visitJumpInsn(IF_ICMPLT, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack2(s, env)
}


case class InstrIF_ICMPNE(targetBlockIdx: Int) extends JumpInstruction {
  /**
    * (Unconditional target, Conditional target)
    * None if next block for unconditional target
    */
  override def getSuccessor(): (Option[Int], Option[Int]) = (None, Some(targetBlockIdx))

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitJumpInsn(IF_ICMPNE, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this))
      mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenINE", genSign(vclasstype, vclasstype, fexprclasstype), false)
    else
      mv.visitJumpInsn(IF_ICMPNE, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack2(s, env)
}


case class InstrIFGT(targetBlockIdx: Int) extends JumpInstruction {
  /**
    * (Unconditional target, Conditional target)
    * None if next block for unconditional target
    */
  override def getSuccessor(): (Option[Int], Option[Int]) = (None, Some(targetBlockIdx))

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitJumpInsn(IFGT, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    if (env.shouldLiftInstr(this))
      mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenGT", genSign(vclasstype, fexprclasstype), false)
    else
      mv.visitJumpInsn(IFGT, env.getBlockLabel(env.getBlock(targetBlockIdx)))
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack1(s, env)
}