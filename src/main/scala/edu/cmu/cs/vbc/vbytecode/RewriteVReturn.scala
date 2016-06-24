package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc.analysis.VBCFrame._
import edu.cmu.cs.vbc.analysis.{VBCFrame, V_TYPE}
import edu.cmu.cs.vbc.utils.LiftUtils
import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.vbytecode.instructions._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{MethodVisitor, Opcodes}

/**
  * assuming for now that a method always returns a V or void. further analysis
  * may determine that a method always returns a nonvariational value.
  *
  * for a method, there are three scenarios regarding throw and return instructions
  * (note that throw instructions might be inserted for caught exceptions in atomic instructions
  * in a prior rewrite step)
  *
  * 1) the method has a single return statement as last instruction and no throw instruction:
  * in this case, a void return can remain and all others return V. No restructuring is necessary.
  * (for now we return One(null) for void methods)
  *
  * 2) In all other cases we need to unify multiple possible exit points by creating a special
  * last block. All throw and return instructions are translated to GOTO statements to
  * the last block, possibly after writing to the $result variable. The instructions in the last
  * block depend on statements in the method:
  *
  * 2a) the method has no return statement but always ends with a throws declaration. In this
  * case, we end with a single throw VException call.
  *
  * 2b) the method has one or more return statements and no throw instructions. In this case
  * the last instruction is a normal return statement for a V
  *
  * 2c) the method has one or more return statements and one or more throw instructions. In this
  * case, the last block returns a special implementation of V (VPartialException) if the
  * $exceptionCond variable is satisfiable and return a normal V otherwise.
  *
  * TODO: if we can ensure that all return statements and all throw instructions occur in
  * nonvariational paths and that all throw instructions will only throw nonvariational
  * exceptions, we do not need to rewrite things and can throw normal exceptions
  */
object RewriteVReturn {


  def ensureUniqueReturnInstr(m: VBCMethodNode): VBCMethodNode = {
    //if the last instruction in the last block is the only return statement and its not a ATHROW, we are happy
    val returnInstr = for (block <- m.body.blocks; instr <- block.instr if instr.isReturnInstr) yield instr.asInstanceOf[ReturnInstruction]
    assert(returnInstr.nonEmpty, "no return instruction found in method")
    assert(checkUniqueReturnType(returnInstr), "inconsistency: different kinds of return instructions found in method")

    val returnInstrWithoutAThrow = returnInstr.filterNot(_.isInstanceOf[InstrATHROW])
    if (isSingleReturn(returnInstr, m.body.blocks.last.instr.last))
      m
    else unifyReturnInstrAndThrow(m: VBCMethodNode, getReturnOpcode(returnInstrWithoutAThrow))
  }


  private def isSingleReturn(returnInstr: List[ReturnInstruction], lastInstr: Instruction): Boolean =
    returnInstr.size == 1 && (returnInstr.head match {
      case InstrATHROW() => false
      case ret => ret == lastInstr
    })

  private def checkUniqueReturnType(returnInstr: List[ReturnInstruction]): Boolean =
    returnInstr.map {
      case InstrATHROW() => -1
      case InstrRETURNVoid() => 0
      case InstrRETURNVal(op) => op
    }.filter(_ >= 0).distinct.size <= 1


  private def getReturnOpcode(returnInstr: List[ReturnInstruction]): Int = {
    var result = -1
    for (op <- returnInstr.map {
      case InstrATHROW() => Opcodes.ATHROW
      case InstrRETURNVoid() => Opcodes.RETURN
      case InstrRETURNVal(op) => op
    }) {
      assert(result == -1 || result == op, "inconsistency: different kinds of return instructions found in method")
      result = op
    }
    result
  }


  private def unifyReturnInstrAndThrow(method: VBCMethodNode, originalReturnOpcode: Int): VBCMethodNode = {
    //TODO technically, all methods will always return type V, so we should not have
    //to worry really about what kind of store/load/return instruction we generate here
    val returnVar = new LocalVar("$result", LiftUtils.vclasstype, LocalVar.initOneNull)
    val exceptionCondVar = new LocalVar("$exceptionCond", LiftUtils.fexprclasstype, LocalVar.initFalse)

    val newReturnBlockIdx = method.body.blocks.size
    var hasThrows = false
    var hasReturn = false


    var substituteInstr: List[Instruction] = List(new InstrGOTO(newReturnBlockIdx))
    if (!method.returnsVoid)
      substituteInstr ::= InstrISTORE(returnVar) //TODO generalize to different types of variables, based on return type

    def substituteReturnInstr(ret: ReturnInstruction): List[Instruction] = ret match {
      case InstrATHROW() =>
        hasThrows = true
        List(VInstrSTORE_Exception(returnVar, exceptionCondVar), InstrGOTO(newReturnBlockIdx))
      case InstrRETURNVoid() =>
        hasReturn = true
        List(InstrGOTO(newReturnBlockIdx))
      case InstrRETURNVal(op) =>
        hasReturn = true
        (op match {
          case Opcodes.IRETURN => InstrISTORE(returnVar)
          case Opcodes.ARETURN => InstrASTORE(returnVar)
          case o => throw new UnsupportedOperationException(s"return $o not supported yet")
        }) :: InstrGOTO(newReturnBlockIdx) :: Nil
    }

    val rewrittenBlocks = method.body.blocks.map(block =>
      new Block(block.instr.flatMap(instr =>
        if (instr.isReturnInstr) substituteReturnInstr(instr.asInstanceOf[ReturnInstruction]) else List(instr)
      ), block.exceptionHandlers))


    assert(hasThrows || hasReturn, "found neither return nor throws statement in method")
    val newReturnBlock = new Block(
      List(VInstrRETURN(returnVar, exceptionCondVar, hasReturn, hasThrows, originalReturnOpcode == Opcodes.RETURN)),
      Nil)


    method.copy(body =
      CFG(
        rewrittenBlocks :+ newReturnBlock
      )
    )
  }
}

object RewriteInvocationExceptionHandling {

  /**
    * TODO detect which blocks are not part of the first VBlock - nodes in the first VBlock can be ignored if their exception handler is empty
    *
    * @param m
    * @return
    */
  private def skipInjection(m: VBCMethodNode) =
    (m.body.blocks.head.exceptionHandlers.isEmpty || !containsInvocation(m.body.blocks.head)) &&
      !m.body.blocks.tail.exists(containsInvocation)


  private def containsInvocation(block: Block) = block.instr.last.isInstanceOf[InstrINVOKESTATIC] || block.instr.last.isInstanceOf[InstrINVOKEVIRTUAL]

  /**
    * for every block that ends in a method call, add an own exception handler for VException
    * (the only exception is when that block is not on a conditional path and did
    * not catch any exceptions before)
    */
  def injectVExceptionHandling(m: VBCMethodNode): VBCMethodNode = if (skipInjection(m)) m
  else {

    var extraBlocks: List[Block] = Nil

    val blocks = for (block <- m.body.blocks) yield
      if (containsInvocation(block)) {
        //add exception handler
        if (block.exceptionHandlers.nonEmpty)
          extraBlocks ::= Block(VInstrDispatchVException(block.exceptionHandlers),InstrATHROW())
        else
          extraBlocks ::= Block(InstrATHROW())
        block.copy(exceptionHandlers = VBCHandler("edu/cmu/cs/varex/VException", m.body.blocks.size + extraBlocks.size - 1) +: block.exceptionHandlers) //TODO check order
      } else block

    m.copy(body = CFG(blocks ++ extraBlocks.reverse)) //TODO check order
  }


}


package instructions {

  import edu.cmu.cs.vbc.analysis.REF_TYPE
  import org.objectweb.asm.Label

  /**
    * special instruction to store an exception in the resultVariable and update the
    * condition in the exceptionConditionVar
    */
  case class VInstrSTORE_Exception(resultVar: LocalVar, exceptionCondVar: LocalVar) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
      throw new UnsupportedOperationException("should not exist in unlifted byte code, created only through rewrite operation")

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
      if (env.shouldLiftInstr(this)) {
        // incoming stack: ..., V<Throwable>
        loadCurrentCtx(mv, env, block) // stack: ..., V<Throwable>, ctx
        mv.visitInsn(SWAP) // stack: ..., ctx, V<Throwable>
        loadV(mv, env, resultVar) // stack: ..., ctx, V<Throwable>, oldResult
        callVCreateChoice(mv) // stack: ..., V<?>
        storeV(mv, env, resultVar) // stack: ...

        loadFExpr(mv, env, exceptionCondVar) // stack: ..., exceptionCond
        loadCurrentCtx(mv, env, block) // stack: ..., exceptionCond, ctx
        callFExprOr(mv) // stack: ..., newExceptionCond
        storeFExpr(mv, env, exceptionCondVar)
      }
      else {
        //in unlifted context, there should be a real throw instruction, not this special store instruction
        throw new UnsupportedOperationException("should never be called in an unlifted context")
      }
    }

    override def getVariables = Set(resultVar, exceptionCondVar)

    override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
      //pops a V<Throwable> and writes to $resultVar and $exceptionCondVar
      env.setLift(this)
      val (value, prev, frame) = s.pop()
      val newFrame = frame.setLocal(resultVar, V_TYPE(), Set(this))
      val backtrack =
        if (value != V_TYPE())
          prev
        else
          Set[Instruction]()
      (newFrame, backtrack)
    }
  }


  /**
    * special variational instruction for the final return or exception in the method
    */
  case class VInstrRETURN(returnVar: LocalVar, exceptionCondVar: LocalVar, hasReturn: Boolean, hasThrows: Boolean, isOriginallyVoidReturn: Boolean) extends ReturnInstruction {
    assert(hasReturn || hasThrows)

    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
      throw new UnsupportedOperationException("VInstrRETURN cannot be used in a nonvariational context; generated only as part of variation rewrite of THROW/RETURN instructions")

    /**
      * see notes for RewriteVReturn
      */
    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
      if (hasThrows && !hasReturn) {
        // only return statement, no need to check whether we have an exception, though we could check to make sure

        //assert that exceptionCondVar.equivalentTo(method-ctx)
        loadCurrentCtx(mv, env, block)
        loadFExpr(mv, env, exceptionCondVar)
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "assertEquivalent", s"($fexprclasstype$fexprclasstype)V", false)

        //actual throw
        throwVException(mv, env)
      } else if (hasReturn && !hasThrows) {
        //only return, no exception; no need for checking $exceptionCondVar
        returnResult(mv, env)
      } else {
        //both throw and return are possible. return partial exception if $exceptionCondVar is satisfiable
        loadFExpr(mv, env, exceptionCondVar)
        callFExprIsSatisfiable(mv)
        val returnLabel = new Label()
        mv.visitJumpInsn(IFEQ, returnLabel) // jump to return instruction if not satisfiable (no conditional jumps here)
        returnVPartialException(mv, env)
        mv.visitLabel(returnLabel)
        returnResult(mv, env)
      }


    }

    def returnResult(mv: MethodVisitor, env: VMethodEnv): Unit =
      if (isOriginallyVoidReturn && env.method.isInit)
        mv.visitInsn(RETURN)
      else if (isOriginallyVoidReturn) {
        mv.visitInsn(ACONST_NULL)
        mv.visitInsn(ARETURN)
      } else {
        loadV(mv, env, returnVar)
        mv.visitInsn(ARETURN)
      }

    def throwVException(mv: MethodVisitor, env: VMethodEnv): Unit = {
      // create VException($exceptionCondVar, $returnVar)
      mv.visitTypeInsn(NEW, vexceptionclassname)
      mv.visitInsn(DUP)
      loadFExpr(mv, env, exceptionCondVar)
      loadV(mv, env, returnVar)
      mv.visitMethodInsn(INVOKESPECIAL, vexceptionclassname, "<init>", s"($fexprclasstype$vclasstype)V", false)
      mv.visitInsn(ATHROW)
    }

    def returnVPartialException(mv: MethodVisitor, env: VMethodEnv): Unit = {
      // create VException($exceptionCondVar, $returnVar)
      mv.visitTypeInsn(NEW, vpartialexceptionclassname)
      mv.visitInsn(DUP)
      loadFExpr(mv, env, exceptionCondVar)
      loadV(mv, env, returnVar)
      mv.visitMethodInsn(INVOKESPECIAL, vpartialexceptionclassname, "<init>", s"($fexprclasstype$vclasstype)V", false)
      mv.visitInsn(ARETURN)
    }

    override def getVariables =
      if (hasThrows) Set(returnVar, exceptionCondVar)
      else if (isOriginallyVoidReturn) Set() else Set(returnVar)

    override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
      (s, Set())

    override def isReturnInstr = true
  }


  case class VInstrDispatchVException(handlers: Seq[VBCHandler]) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit =
      throw new UnsupportedOperationException("VInstrDispatchVException cannot be used in a nonvariational context; generated only as part of variation rewrite of invocation instructions")

    override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
      val (v, prev, newFrame) = s.pop()
      (newFrame.push(V_TYPE(), Set()), Set())
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
      //stack: [VException]
      mv.visitInsn(DUP)
      //stack: [VException,VException]
      mv.visitMethodInsn(INVOKEVIRTUAL, "edu/cmu/cs/varex/VException", "getExceptionIterator", "()Ljava/util/Iterator;", false)
      //stack: [VException,Iterator]
      val loopLabel = new Label()
      mv.visitLabel(loopLabel)

      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/Iterator", "next", "()Ledu/cmu/cs/varex/VException/ExceptionConditionPair;", false)
      //stack: [VException,Iterator,ExceptionConditionPair]

      mv.visitInsn(DUP)
      mv.visitFieldInsn(GETFIELD, "edu/cmu/cs/varex/VException/ExceptionConditionPair", "exception", "Ljava/lang/Throwable;")
      //stack: [VException,Iterator,ExceptionConditionPair,Throwable]

      for (handler <- handlers) {
        //stack: [VException,Iterator,ExceptionConditionPair,Throwable]
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Throwable", "getClass", "()Ljava/lang/Class;", false)
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Class", "getCanonicalName", "()Ljava/lang/String;", false)
        //stack: [VException,Iterator,ExceptionConditionPair,Throwable,String(exceptionname)]

        mv.visitLdcInsn(handler.exceptionType)
        //stack: [VException,Iterator,ExceptionConditionPair,Throwable,String(exceptionname),String(handledException)]
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/String", "equals", "(Ljava/lang/Object;)Z", false)
        //stack: [VException,Iterator,ExceptionConditionPair,Throwable,boolean]
        val jumpOverLabel = new Label()
        mv.visitJumpInsn(IFNE, jumpOverLabel)
        //stack: [VException,Iterator,ExceptionConditionPair,Throwable]

        //ctx = ctx andNot exception.cond
        mv.visitInsn(SWAP)
        //stack: [VException,Iterator,Throwable,ExceptionConditionPair]
        mv.visitInsn(DUP)
        mv.visitFieldInsn(GETFIELD, "edu/cmu/cs/varex/VException/ExceptionConditionPair", "cond", fexprclasstype)
        //stack: [VException,Iterator,Throwable,ExceptionConditionPair,FeatureExpr]
        mv.visitInsn(DUP)
        callFExprNot(mv)
        loadCurrentCtx(mv, env, block)
        callFExprAnd(mv)
        storeCurrentCtx(mv, env, block)
        //stack: [VException,Iterator,Throwable,ExceptionConditionPair,FeatureExpr]

        val targetBlock = env.getBlock(handler.handlerBlockIdx)
        val targetBlockCtx = env.getVBlockVar(targetBlock)
        loadFExpr(mv, env, targetBlockCtx)
        callFExprOr(mv)
        storeFExpr(mv, env, targetBlockCtx)
        //stack: [VException,Iterator,Throwable,ExceptionConditionPair]

        mv.visitInsn(SWAP)
        //stack: [VException,Iterator,ExceptionConditionPair,Throwable]
        mv.visitInsn(DUP)
        assert(env.getExpectingVars(targetBlock).size == 1, "exception blocks may only expect exactly one Exception variable")
        mv.visitVarInsn(ASTORE, env.getVarIdx(env.getExpectingVars(targetBlock).head))

        mv.visitLabel(jumpOverLabel)
        //stack: [VException,Iterator,ExceptionConditionPair,Throwable]
      }
      mv.visitInsn(POP)
      mv.visitInsn(POP)
      //stack: [VException,Iterator]
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/util/Iterator", "hasNext", "()Z", false)
      //stack: [VException,Iterator,boolean(hasNext)]
      mv.visitJumpInsn(IFNE, loopLabel)
      mv.visitInsn(POP)
      //stack: [VException]
      mv.visitMethodInsn(INVOKEVIRTUAL, "edu/cmu/cs/varex/VException", "getException", "()" + vclasstype, false)
      //stack: [V<Exception>]
    }
  }

}