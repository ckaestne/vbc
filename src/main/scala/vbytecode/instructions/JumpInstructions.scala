package edu.cmu.cs.vbc.vbytecode.instructions

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

        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenEQ", "(Ledu/cmu/cs/varex/V;)Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
        //only evaluate condition, jump in block implementation
    }

    override def getSuccessor() = (None, Some(targetBlockIdx))
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
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenNE", "(Ledu/cmu/cs/varex/V;)Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
    }
}


case class InstrGOTO(targetBlockIdx: Int) extends JumpInstruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        val targetBlock = env.getBlock(targetBlockIdx)
        mv.visitJumpInsn(GOTO, env.getBlockLabel(targetBlock))
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        //handled in block implementation
    }


    override def getSuccessor() = (Some(targetBlockIdx), None)
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
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenIEQ", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
    }
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
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenIGE", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
    }
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
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenGE", genSign(vclasstype, fexprclasstype), false)
    }
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
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenILT", genSign(vclasstype, vclasstype, fexprclasstype), false)
    }
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
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenGT", genSign(vclasstype, fexprclasstype), false)
    }
}
