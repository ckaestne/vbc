package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._


/**
  * the design for control-flow lifting is as follows:
  *
  * every block (node in the CFG) has a condition that
  * is initially FALSE
  *
  * when executing the method, the first block receives
  * the ctx of the method as condition.
  *
  * when executing a block with a contradictory condition,
  * one can jump to the next block
  *
  * after executing a block, this blocks condition is
  * set to FALSE and the successor block's conditions are
  * updated:
  * - if there is an unconditional jump, the successor
  * block's condition is "successor.condition or thisblock.condition"
  * - if there is a condition that evaluates to true
  * under condition A, the then-successor's condition is
  * "then-successor.condition or (thisblock.condition and A)"
  * and the else-successor's condition is
  * "else-successor.condition or (thisblock.condition andNot A)"
  *
  * If either successor is before the current block and has
  * a satisfiable condition, we jump back to the successor that
  * is further back.
  * If neither successor is before the current block, we
  * proceed execution with the next block (not necessarily
  * a successor). Note that only at most one successor can be before
  * the current block.
  *
  *
  *
  * The intuition behind this approach is as follows:
  *
  * - we may execute everything multiple times, but we can
  * split the context and the next block to execute might
  * be in different locations.
  *
  * - two blocks with mutually exclusive conditions can be
  * executed in any order
  *
  * - the current approach always jumps back to the earliest
  * block that has a satisfiable condition. As after
  * execution each block turns FALSE, we can just move
  * forward until we make a block further back satisfiable -- that's
  * where we jump to
  *
  * - we could fork the execution on every decision and
  * execute the function to the end, jumping back from
  * there to the last decision and exploring the next
  * path under a missing condition, but this would forgo
  * any joining.
  *
  * - we could also just go sequentially through the
  * method multiple times until all blocks of a contradictory
  * condition, but we may forgot sharing opportunities;
  * we can also resort blocks for that purpose if we stick
  * to the same strategy;
  * there might also be a more efficient mechanism that
  * we can chose at runtime, for example executing those
  * blocks that are least merged yet, jumping back and forth
  * until all blocks have contradictory conditions; or there might even
  * be a static strategy to predict where joining is most
  * likely
  * TODO study this empirically
  *
  * Notes
  *
  * - each block should have either have more than one successor
  * or more than one predecessor. otherwise blocks can be merged.
  * TODO exploit this for optimization
  *
  */









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

    override def toVByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {

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


case class InstrGOTO(targetBlockIdx: Int) extends JumpInstruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        val targetBlock = env.getBlock(targetBlockIdx)
        mv.visitJumpInsn(GOTO, env.getBlockLabel(targetBlock))
    }

    override def toVByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        //handled in block implementation
    }


    override def getSuccessor() = (Some(targetBlockIdx), None)
}

case class Block(instr: Instruction*) extends LiftUtils {


    def toByteCode(mv: MethodVisitor, env: MethodEnv) = {
        validate()

        mv.visitLabel(env.getBlockLabel(this))
        instr.foreach(_.toByteCode(mv, env, this))
    }

    def toVByteCode(mv: MethodVisitor, env: MethodEnv) = {
        mv.visitLabel(env.getBlockLabel(this))

        val thisBlockConditionVar = env.getBlockVar(this)
        val nextBlock = env.getNextBlock(this)

        //load block condition (local variable for each block)
        //jump to next block if condition is contradictory
        if (nextBlock.isDefined) {
            loadFExpr(mv, env, thisBlockConditionVar)
            //            mv.visitInsn(DUP)
            //            mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printFE", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)V", false)
            callFExprIsContradiction(mv)
            //            mv.visitInsn(DUP)
            //            mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printI", "(I)V", false)
            mv.visitJumpInsn(IFNE, env.getBlockLabel(nextBlock.get))
        }

        //generate block code
        instr.foreach(_.toVByteCode(mv, env, this))

        val successors = env.getSuccessors(this)
        if (successors._1 == None) {
            //TODO deal with last block
        } else if (successors._2 == None) {
            val targetBlock = successors._1.get
            val targetBlockConditionVar = env.getBlockVar(targetBlock)
            //if non-conditional jump
            //- update next block's condition (disjunction with prior value)
            loadFExpr(mv, env, thisBlockConditionVar)
            loadFExpr(mv, env, targetBlockConditionVar)
            callFExprOr(mv)
            storeFExpr(mv, env, targetBlockConditionVar)

            //- set this block's condition to FALSE
            pushConstantFALSE(mv)
            storeFExpr(mv, env, thisBlockConditionVar)

            //- if backward jump, jump there (target condition is satisfiable, because this block's condition is and it's propagated)
            if (env.isBlockBefore(targetBlock, this)) {
                mv.visitJumpInsn(GOTO, env.getBlockLabel(targetBlock))
            }

        } else {
            //if conditional jump (then the last instruction left us a featureexpr on the stack)
            val thenBlock = successors._2.get
            val thenBlockConditionVar = env.getBlockVar(thenBlock)
            val elseBlock = successors._1.get
            val elseBlockConditionVar = env.getBlockVar(elseBlock)
            mv.visitInsn(DUP)
            // -- stack: 2x Fexpr representing if condition

            //- update else-block's condition (ie. next block)
            callFExprNot(mv)
            loadFExpr(mv, env, thisBlockConditionVar)
            callFExprAnd(mv)
            loadFExpr(mv, env, elseBlockConditionVar)
            callFExprOr(mv)
            storeFExpr(mv, env, elseBlockConditionVar)


            //- update then-block's condition to "then-successor.condition or (thisblock.condition and A)"
            loadFExpr(mv, env, thisBlockConditionVar)
            callFExprAnd(mv)
            loadFExpr(mv, env, thenBlockConditionVar)
            callFExprOr(mv)
            if (env.isBlockBefore(thenBlock, this))
                mv.visitInsn(DUP)
            storeFExpr(mv, env, thenBlockConditionVar)

            //- set this block's condition to FALSE
            pushConstantFALSE(mv)
            storeFExpr(mv, env, thisBlockConditionVar)

            //            for (block<-method.body.blocks) {
            //                mv.visitVarInsn(ALOAD, block.blockConditionVar)
            //                mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printFE", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)V", false)
            //            }

            //- if then-block is behind and its condition is satisfiable, jump there
            if (env.isBlockBefore(thenBlock, this)) {
                //value remembered with DUP up there to avoid loading it again
                callFExprIsSatisfiable(mv)
                mv.visitJumpInsn(IFNE, env.getBlockLabel(thenBlock))
            }
        }

    }


    def validate(): Unit = {
        // ensure last statement is the only jump instruction, if any
        instr.dropRight(1).foreach(i => {
            assert(!i.isJumpInstr, "only the last instruction in a block may be a jump instruction (goto, if)")
            assert(!i.isReturnInstr, "only the last instruction in a block may be a return instruction")
        })
    }

    def vvalidate(env: MethodEnv): Unit = {
        validate()
        //additionally ensure that the last block is the only one that contains a return statement
        if (this != env.getLastBlock())
            assert(!instr.last.isReturnInstr, "only the last block may contain a return instruction in variational byte code")
    }

    def getVar(env: MethodEnv): Variable = env.getBlockVar(this)


    override def equals(that: Any): Boolean = that match {
        case t: Block => t eq this
        case _ => false
    }




}


case class CFG(blocks: List[Block]) extends LiftUtils {


    def toByteCode(mv: MethodVisitor, env: MethodEnv) = {
        blocks.foreach(_.toByteCode(mv, env))
    }


    def toVByteCode(mv: MethodVisitor, env: MethodEnv) = {
        // allocate a variable for each block, except for the first, which can reuse the parameter slot
        blocks.headOption.map(env.setBlockVar(_, env.ctxParameter))
        blocks.tail.foreach(env.setBlockVar(_, env.freshLocalVar()))

        // initialize all block variables to FALSE, except for the first one which is initialized
        // to the ctx parameter (by using the parameter's slot in the stack)
        for (block <- blocks.tail) {
            pushConstantFALSE(mv)
            storeFExpr(mv, env, env.getBlockVar(block))
        }

        //there might be a smarter way, but as we need to load an old value when
        //conditionally storing an updated value, we need to initialize all lifted
        //fields. here setting them all to null
        //the same process occurs (not actually but as a potential case for the
        //analysis when jumping over unsatisfiable blocks)
        for (v <- env.getLocalVariables()) {
            mv.visitInsn(ACONST_NULL)
            storeV(mv, env, v)
        }

        blocks.foreach(_.toVByteCode(mv, env))
    }
}