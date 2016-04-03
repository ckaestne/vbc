package edu.cmu.cs.vbc.vbytecode

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

  def rewriteV(m: VBCMethodNode): VBCMethodNode =
    initializeConditionalFields(
      RewriteVReturn.ensureUniqueReturnInstr(m)
    )


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
      val firstInstr = initialInstr.head
      val secondInstr = initialInstr.tail.head
      assert(firstInstr.isALOAD0, "first instruction in <init> is not ALOAD0")
      assert(secondInstr.isINVOKESPECIAL_OBJECT_INIT,
        "second instruction in <inti> is not INVOKESPECIAL java/lang/Object.<init> ()V")

      // ALOAD and INVOKESPECIAL will be inserted in CFG
      val newInstrs = nopPrefix ++ (InstrINIT_CONDITIONAL_FIELDS() +: initialInstr.drop(2))
      val newBlocks = new Block(newInstrs: _*) +: m.body.blocks.drop(1)
      m.copy(body = CFG(newBlocks))
    } else m

}


