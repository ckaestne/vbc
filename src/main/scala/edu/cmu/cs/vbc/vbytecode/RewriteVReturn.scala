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
  * case, the last block throws a VException if the $exceptionCond variable is satisfiable and
  * uses a return statement otherwise.
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
    returnInstr.map(_ match {
      case InstrATHROW() => -1
      case InstrRETURNVoid() => 0
      case InstrRETURNVal(op) => op
    }).filter(_ >= 0).distinct.size <= 1


  private def getReturnOpcode(returnInstr: List[ReturnInstruction]): Int = {
    var result = -1
    for (op <- returnInstr.map(_ match {
      case InstrATHROW() => Opcodes.ATHROW
      case InstrRETURNVoid() => Opcodes.RETURN
      case InstrRETURNVal(op) => op
    })) {
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
      ): _*))


    assert(hasThrows || hasReturn, "found neither return nor throws statement in method")
    val newReturnBlock = new Block(VInstrRETURN(returnVar, exceptionCondVar, hasReturn, hasThrows, originalReturnOpcode == Opcodes.RETURN))


    method.copy(body =
      CFG(
        rewrittenBlocks :+ newReturnBlock
      )
    )
  }
}


package instructions {

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
  case class VInstrRETURN(returnVar: LocalVar, exceptionCondVar: LocalVar, hasReturn: Boolean, hasThrows: Boolean, isVoidReturn: Boolean) extends Instruction {
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
        throwVException(mv, env)
      } else if (hasReturn && !hasThrows) {
        //only return, no exception; no need for checking $exceptionCondVar
        returnResult(mv, env)
      } else {
        //both throw and return are possible. throw exception if $exceptionCondVar is satisfiable
        loadFExpr(mv, env, exceptionCondVar)
        callFExprIsSatisfiable(mv)
        val returnLabel = new Label()
        mv.visitJumpInsn(IFEQ, returnLabel) // jump to return instruction if not satisfiable (no conditional jumps here)
        throwVException(mv, env)
        mv.visitLabel(returnLabel)
        returnResult(mv, env)
      }


    }

    def returnResult(mv: MethodVisitor, env: VMethodEnv): Unit =
      if (isVoidReturn)
        mv.visitInsn(RETURN)
      else {
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

    override def getVariables = if (hasThrows) Set(returnVar, exceptionCondVar)
    else if (isVoidReturn) Set() else Set(returnVar)

    override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
      (s, Set())

    override def isReturnInstr = true
  }


}