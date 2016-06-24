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
      RewriteVReturn.ensureUniqueReturnInstr(
        RewriteInvocationExceptionHandling.injectVExceptionHandling(
          addHandlerForAtomicExceptions(
            ensureUniqueHandlers(m)))))


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

  /**
    * add an exception handler for every block with instructions that may throw
    * (atomic) exceptions. The exception handler is simple in that it just
    * rethrows the exception. By construction, it belongs to the same VBlock,
    * but we can now exploit the VThrow mechanism from a subsequent rewrite.
    * The new handlers are added to the end, in case that there is already
    * a handler for that exception
    *
    * TODO: this transformation is not necessary for a block in the method's
    * context (would require tagV analysis)
    *
    * TODO: it might make things much easier if each VBlock has its own
    * exception handler. As a simple mechanism, we could create exception
    * handlers for each block that point to the original exception handler
    * with a GOTO, then the GOTO could be handled with the classic mechanism.
    * Again, having an early tagV analysis would be helpful, as only one
    * exception handler per vblock would be necessary
    */
  private def addHandlerForAtomicExceptions(m: VBCMethodNode): VBCMethodNode = {
    def liftBody(blocks: List[Block]): List[Block] = {
      var newBlocks: List[Block] = Nil

      val updatedBlocks = blocks.map(block => {
        val atomicExceptionsInBlock = block.instr.foldLeft(Set[String]())(_ ++ _.atomicExceptions)
        val existingExceptionHandlers = block.exceptionHandlers.map(_.exceptionType)
        val additionalExceptions = (atomicExceptionsInBlock -- existingExceptionHandlers)
        if (additionalExceptions.isEmpty)
          block
        else {
          val newBlockIdx = blocks.size + newBlocks.size
          newBlocks ::= Block(InstrATHROW())
          val newHandlers = for (ex <- additionalExceptions) yield VBCHandler(ex, newBlockIdx)
          Block(block.instr, block.exceptionHandlers ++ newHandlers)
        }
      })

      updatedBlocks ++ newBlocks.reverse
    }


    m.copy(body = CFG(liftBody(m.body.blocks)))
  }

  /**
    * each exception handler must be in the same VBlock as the block throwing the exception,
    * which means that two blocks in different VBlocks cannot share an exception handler.
    *
    * We fix with with a simple solution, for each exception handler, we introduce a new
    * block with a single jump instruction to the original exception handling block. thus
    * every block has its own exception handler and the jump from those new exception handlers
    * to the old ones can be done with the normal rewriting of conditional control flow.
    *
    * An additional issue is that an exception block cannot be a jump target across VBlocks.
    * We simply count jump edges just as exception edges and create additional exception blocks
    * whenever we have both a jump and an exception edge to a block.
    *
    * TODO this is currently conservative and introduces unnecessary jumps if multiple blocks
    * in the same VBlock share an exception handler. Those can be easily shared because
    * they are within the same VBlock and always have the same context. Unfortunately, we do not have easy
    * access to VBlocks here, but we can later remove the unnecessary blocks that contain just
    * a single jump instruction after all.
    **/
  private def ensureUniqueHandlers(m: VBCMethodNode): VBCMethodNode = {
    val allJumpTargets: Seq[Int] =
      (m.body.blocks.indices zip m.body.blocks).flatMap(x => {
        val y = x._2.getJumpTargets(Some(x._1 + 1))
        y._1.toList ++ y._2.toList
      })
    val sharedExceptionHandlerTargets =
      (m.body.blocks.flatMap(_.exceptionHandlers.map(_.handlerBlockIdx).distinct) ++ allJumpTargets).
        groupBy(identity).filter(_._2.length > 1).map(_._1).toSet

    def liftBody(blocks: List[Block]): List[Block] = {
      var newBlocks: List[Block] = Nil

      def updateHandler(handler: VBCHandler): VBCHandler = {
        if (sharedExceptionHandlerTargets contains handler.handlerBlockIdx) {
          val newBlockIdx = blocks.size + newBlocks.size
          newBlocks ::= Block(InstrGOTO(handler.handlerBlockIdx))
          handler.copy(handlerBlockIdx = newBlockIdx)
        } else handler
      }

      val updatedBlocks = blocks.map(b => b.copy(exceptionHandlers = b.exceptionHandlers.map(updateHandler)))

      updatedBlocks ++ newBlocks.reverse
    }


    m.copy(body = CFG(liftBody(m.body.blocks)))

  }


}


