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


  /* Assume that the first two instructions in the <init> method are:
       ALOAD 0
       INVOKESPECIAL java/lang/Object.<init> ()V

     Inject a new instruction to initialize fields in the beginning of the subsequent new block

     TODO: should initialize static fields earlier?
  */
  private def initializeConditionalFields(m: VBCMethodNode): VBCMethodNode =
    if (m.isInit) {
      val firstBlock = m.body.blocks.head
      val firstBlockInstructions = firstBlock.instr

      val nopPrefix = firstBlockInstructions.takeWhile(_.isInstanceOf[EmptyInstruction])
      val initialInstr = firstBlockInstructions.drop(nopPrefix.length)
      val firstInstr = initialInstr.head
      val secondInstr = initialInstr.tail.head
      assert(firstInstr.isALOAD0, "first instruction in <init> is not ALOAD0")
      assert(secondInstr.isINVOKESPECIAL_OBJECT_INIT,
        "second instruction in <inti> is not INVOKESPECIAL java/lang/Object.<init> ()V")
      assert(initialInstr.size == 2, "did not expect additional statements after ALOAD0 and INVOKESPECIAL")

      val secondBlock = m.body.blocks.tail.head
      assert(secondBlock != null, "expected second block in <init> at least with RETURN instruction")
      val newBlocks = firstBlock :: secondBlock.copy(instr = InstrINIT_CONDITIONAL_FIELDS() +: secondBlock.instr) +: m.body.blocks.drop(2)
      m.copy(body = CFG(newBlocks))
    } else m

}


