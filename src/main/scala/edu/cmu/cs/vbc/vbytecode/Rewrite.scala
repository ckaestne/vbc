package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc.utils.LiftUtils
import edu.cmu.cs.vbc.vbytecode.instructions._


/**
  * rewrites of methods as part of variational lifting.
  * note that ASTs are immutable; returns a new (rewritten) method.
  *
  * rewrites happen before toVByteCode is called on a method;
  * the MethodEnv can be used for the transformation
  */
object Rewrite {


  def rewrite(m: VBCMethodNode, cls: VBCClassNode): VBCMethodNode =
    initializeConditionalFields(m, cls)

  def rewriteV(m: VBCMethodNode, cls: VBCClassNode): VBCMethodNode = {
    if (m.body.blocks.nonEmpty) {
//      profiling(
        initializeConditionalFields(
          appendGOTO(
            ensureUniqueReturnInstr(
              replaceAthrowWithAreturn(m)
            )
          ), cls
      )
//        , cls)
    }
    else {
      m
    }
  }

  var idCount = 0
  private def profiling(m: VBCMethodNode, cls: VBCClassNode): VBCMethodNode = {
    val id = cls.name + "#" + m.name + "#" + idCount
    idCount += 1
    val newBlocks = m.body.blocks.map(b =>
      if (b == m.body.blocks.head)
        Block(InstrStartTimer(id) +: b.instr, b.exceptionHandlers)
      else if (b == m.body.blocks.last)
        Block(b.instr.flatMap(i =>
          if (i.isReturnInstr)
            List(InstrStopTimer(id), i)
          else
            List(i)
        ), b.exceptionHandlers)
      else
        b
    )
    m.copy(body = CFG(newBlocks))
  }

  private def appendGOTO(m: VBCMethodNode): VBCMethodNode = {
    val rewrittenBlocks = m.body.blocks.map(b =>
      if (b != m.body.blocks.last && b.instr.nonEmpty && !b.instr.last.isJumpInstr) {
        Block(b.instr :+ InstrGOTO(m.body.blocks.indexOf(b) + 1), b.exceptionHandlers)
      } else
        b
    )
    m.copy(body = CFG(rewrittenBlocks))
  }

  private def replaceAthrowWithAreturn(m: VBCMethodNode): VBCMethodNode = {
    val rewrittenBlocks = m.body.blocks.map(b =>
      Block(b.instr.flatMap(i => List(
        if (i.isATHROW) InstrARETURN() else i)
      ), b.exceptionHandlers)
    )
    m.copy(body = CFG(rewrittenBlocks))
  }


  private def ensureUniqueReturnInstr(m: VBCMethodNode): VBCMethodNode = {
    //if the last instruction in the last block is the only return statement, we are happy
    val returnInstr = for (block <- m.body.blocks; instr <- block.instr if instr.isReturnInstr) yield instr
    assert(returnInstr.nonEmpty, "no return instruction found in method")
    // RETURN and ARETURN should not happen together, otherwise code could not compile in the first place.
    // For all other kinds of return instructions that take argument, there is no need to worry about exact return type
    // because they are all Vs anyway.
//    assert(returnInstr.map(_.getClass).distinct.size == 1, "inconsistency: different kinds of return instructions found in method")
    if (returnInstr.size == 1 && returnInstr.head == m.body.blocks.last.instr.last)
      m
    else {
      unifyReturnInstr(m: VBCMethodNode, if (m.name == "<init>") InstrRETURN() else InstrARETURN())
    }
  }

  private def unifyReturnInstr(method: VBCMethodNode, returnInstr: Instruction): VBCMethodNode = {
    //TODO technically, all methods will always return type V, so we should not have
    //to worry really about what kind of store/load/return instruction we generate here
    val returnVariable = new LocalVar("$result", LiftUtils.vclasstype)

    var newReturnBlockInstr = List(returnInstr)
    if (!returnInstr.isRETURN)
      newReturnBlockInstr ::= InstrALOAD(returnVariable)
    val newReturnBlock = Block(newReturnBlockInstr, Nil)
    val newReturnBlockIdx = method.body.blocks.size

    def storeAndGoto: List[Instruction] = List(InstrASTORE(returnVariable), InstrGOTO(newReturnBlockIdx))
    def storeNullAndGoto: List[Instruction] = InstrACONST_NULL() :: storeAndGoto

    val rewrittenBlocks = method.body.blocks.map(block =>
      Block(block.instr.flatMap(instr =>
        if (instr.isReturnInstr) {
          if (instr.isRETURN) storeNullAndGoto else storeAndGoto
        }
        else
          List(instr)
      ), block.exceptionHandlers))

    method.copy(body =
      CFG(
        rewrittenBlocks :+ newReturnBlock
      )
    )

  }


  /** Insert INIT_CONDITIONAL_FIELDS in init
    *
    * Current rewriting assumes following sequence is in the first block of init:
    *
    * ALOAD 0
    * {load arguments}
    * INVOKESPECIAL superclass's init
    *
    */
  private def initializeConditionalFields(m: VBCMethodNode, cls: VBCClassNode): VBCMethodNode =
    if (m.isInit) {
      val firstBlockInstructions = m.body.blocks.head.instr
      val newBlocks = Block(InstrINIT_CONDITIONAL_FIELDS() +: firstBlockInstructions, Nil) +: m.body.blocks.drop(1)
      m.copy(body = CFG(newBlocks))
    } else m

}
