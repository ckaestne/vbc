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


  def rewrite(m: VBCMethodNode): VBCMethodNode =
    initializeConditionalFields(m)

  def rewriteV(m: VBCMethodNode): VBCMethodNode = {
    if (m.body.blocks.nonEmpty) {
      initializeConditionalFields(
        ensureUniqueReturnInstr(m)
      )
    }
    else {
      m
    }
  }


  private def ensureUniqueReturnInstr(m: VBCMethodNode): VBCMethodNode = {
    //if the last instruction in the last block is the only return statement, we are happy
    val returnInstr = for (block <- m.body.blocks; instr <- block.instr if instr.isReturnInstr) yield instr
    assert(returnInstr.nonEmpty, "no return instruction found in method")
    assert(returnInstr.map(_.getClass).distinct.size == 1, "inconsistency: different kinds of return instructions found in method")
    if (returnInstr.size == 1 && returnInstr.head == m.body.blocks.last.instr.last)
      m
    else unifyReturnInstr(m: VBCMethodNode, returnInstr.head)
  }

  private def unifyReturnInstr(method: VBCMethodNode, returnInstr: Instruction): VBCMethodNode = {
    //TODO technically, all methods will always return type V, so we should not have
    //to worry really about what kind of store/load/return instruction we generate here
    val returnVariable = new LocalVar("$result", LiftUtils.vclasstype)

    var newReturnBlockInstr = List(returnInstr)
    if (!method.returnsVoid)
      newReturnBlockInstr ::= InstrILOAD(returnVariable) //TODO generalize to different types of variables, based on return type
    val newReturnBlock = new Block(newReturnBlockInstr: _*)
    val newReturnBlockIdx = method.body.blocks.size

    var substituteInstr: List[Instruction] = List(new InstrGOTO(newReturnBlockIdx))
    if (!method.returnsVoid)
      substituteInstr ::= InstrISTORE(returnVariable) //TODO generalize to different types of variables, based on return type

    val rewrittenBlocks = method.body.blocks.map(block =>
      new Block(block.instr.flatMap(instr =>
        if (instr.isReturnInstr) substituteInstr else List(instr)
      ): _*))

    method.copy(body =
      CFG(
        rewrittenBlocks :+ newReturnBlock
      )
    )

  }


  private def initializeConditionalFields(m: VBCMethodNode): VBCMethodNode =
    if (m.isInit) {
      val firstBlock = m.body.blocks.head
      val firstBlockInstructions = firstBlock.instr

      /* Assume that the first two instructions in the <init> method are:
           ALOAD 0
           INVOKESPECIAL java/lang/Object.<inti> ()V
      */
      val nopPrefix = firstBlockInstructions.takeWhile(_.isInstanceOf[EmptyInstruction])
      val initialInstr = firstBlockInstructions.drop(nopPrefix.length)

      val pairs = initialInstr.init zip initialInstr.tail // each pair is two consecutive instructions
      val pair = pairs.find((tuple) => tuple._1.isALOAD0 && tuple._2.isINVOKESPECIAL_OBJECT_INIT)
      assert(pair.isDefined, "no ALOAD0 and INVOKESPECIAL sequence in <init> method")
      val count = pairs.count(_ == pair.get)
      assert(count == 1, "more than one ALOAD0 and INVOKESPECIAL sequence in <init> method")

      val restInstrs = initialInstr.filter((i) => i != pair.get._1 && i != pair.get._2)

      // ALOAD and INVOKESPECIAL will be inserted in CFG
      val newInstrs = nopPrefix ++ (InstrINIT_CONDITIONAL_FIELDS() +: restInstrs)
      val newBlocks = new Block(newInstrs: _*) +: m.body.blocks.drop(1)
      m.copy(body = CFG(newBlocks))
    } else m

}
