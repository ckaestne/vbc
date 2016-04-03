package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc.utils.LiftUtils
import edu.cmu.cs.vbc.vbytecode.instructions._
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._


/**
  * for design rationale, see https://github.com/ckaestne/vbc/wiki/ControlFlow
  */


case class Block(instr: Instruction*) {

  import LiftUtils._

  def toByteCode(mv: MethodVisitor, env: MethodEnv) = {
    validate()

    mv.visitLabel(env.getBlockLabel(this))
    instr.foreach(_.toByteCode(mv, env, this))
  }

  def toVByteCode(mv: MethodVisitor, env: VMethodEnv) = {
    vvalidate(env)
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

    //load local variables if this block is expecting some values on stack
    val expectingVars = env.getExpectingVars(this)
    if (expectingVars.nonEmpty) {
      expectingVars.foreach(
        (v: Variable) => {
          mv.visitVarInsn(ALOAD, env.getVarIdx(v))
        }
      )
    }

    //generate block code
    instr.foreach(_.toVByteCode(mv, env, this))

    //store local variables if this block is leaving some values on stack
    val leftVars = env.getLeftVars(this)
    if (leftVars.nonEmpty) {
      var hasFEOnTop = false
      if (instr.last.isJumpInstr) {
        val j = instr.last.asInstanceOf[JumpInstruction]
        val (uncond, cond) = j.getSuccessor()
        if (cond.isDefined) {
          // conditional jump, which means there is a FE on the stack right now
          hasFEOnTop = true
        }
      }
      leftVars.reverse.foreach(
        (s: Set[Variable]) => {
          if (hasFEOnTop) mv.visitInsn(SWAP)
          s.size match {
            case 1 => {
              val v = s.toList.head
              loadFExpr(mv, env, env.getBlockVar(this))
              mv.visitInsn(SWAP)
              mv.visitVarInsn(ALOAD, env.getVarIdx(v))
              callVCreateChoice(mv)
              mv.visitVarInsn(ASTORE, env.getVarIdx(v))
            }
            case 2 => {
              val list = s.toList
              val v1 = list.head
              val v2 = list.last
              mv.visitInsn(DUP)
              loadFExpr(mv, env, env.getBlockVar(this))
              mv.visitInsn(SWAP)
              mv.visitVarInsn(ALOAD, env.getVarIdx(v1))
              callVCreateChoice(mv)
              mv.visitVarInsn(ASTORE, env.getVarIdx(v1))
              loadFExpr(mv, env, env.getBlockVar(this))
              mv.visitInsn(SWAP)
              mv.visitVarInsn(ALOAD, env.getVarIdx(v2))
              callVCreateChoice(mv)
              mv.visitVarInsn(ASTORE, env.getVarIdx(v2))
            }
            case v => throw new RuntimeException(s"size of Set[Variable] is $v, but expected 1 or 2")
          }
        }
      )
    }


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

  def vvalidate(env: VMethodEnv): Unit = {
    validate()
    //additionally ensure that the last block is the only one that contains a return statement
    if (this != env.getLastBlock())
      assert(!instr.last.isReturnInstr, "only the last block may contain a return instruction in variational byte code")
  }


  override def equals(that: Any): Boolean = that match {
    case t: Block => t eq this
    case _ => false
  }


}


case class CFG(blocks: List[Block]) {

  def toByteCode(mv: MethodVisitor, env: MethodEnv) = {
    // For <init> methods, the first two instructions should be ALOAD 0 and INVOKESPECIAL
    if (env.method.isInit) {
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    }
    blocks.foreach(_.toByteCode(mv, env))
  }


  def toVByteCode(mv: MethodVisitor, env: VMethodEnv) = {

    // For <init> methods, the first two instructions should be ALOAD 0 and INVOKESPECIAL
    if (env.method.isInit) {
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    }




    var initializeVars: List[LocalVar] = Nil

    //initialize all fresh variables (e.g., used for result, unbalanced stacks, exceptionCond, blockCondition)
    initializeVars ++= env.getFreshVars()

    //there might be a smarter way, but as we need to load an old value when
    //conditionally storing an updated value, we need to initialize all lifted
    //fields. here setting them all to One(null)
    //the same process occurs (not actually but as a potential case for the
    //analysis when jumping over unsatisfiable blocks)
    initializeVars ++= env.getLocalVariables()

    for (v <- initializeVars.distinct)
      v.vinitialize(mv, env, v)


    blocks.foreach(_.toVByteCode(mv, env))
  }
}