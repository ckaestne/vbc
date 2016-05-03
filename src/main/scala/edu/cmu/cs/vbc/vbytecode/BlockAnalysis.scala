package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc.vbytecode.instructions.{JumpInstruction, ReturnInstruction}

import scala.collection.immutable.Queue

/**
  * Created by ckaestne on 4/5/2016.
  */

/**
  * analysis is all done on the original byte code and does not include
  * extra edges for catching exceptions not caught in the original implementation
  */
trait CFGAnalysis {

  protected def method: VBCMethodNode

  protected def blocks = method.body.blocks

  private val (succ, pred) = {
    var succ: Map[Block, Set[Block]] = Map()
    var pred: Map[Block, Set[Block]] = blocks.map((_, Set[Block]())).toMap
    for (block <- blocks) {
      val lastInstr = block.instr.last
      val blockIdx = getBlockIdx(block)

      var next: Set[Block] = Set()
      lastInstr match {
        case jump: JumpInstruction =>
          // all possible jump targets are variational, exception edges are not
          val succ = jump.getSuccessor()
          next += getBlock(succ._1.getOrElse(blockIdx + 1))
          succ._2.foreach(next += getBlock(_))
        case r: ReturnInstruction => // nothing
        case _ =>
          // fall through
          next += getBlock(blockIdx + 1)
        //          assert(false, s"expected jump instruction as last instruction of a block, but found $lastInstr")
      }

      succ += (block -> next)
      for (n <- next)
        pred += (n -> (pred.getOrElse(n, Set()) + block))
    }
    for (block <- blocks; handler <- block.exceptionHandlers) {
      val handlerBlock = getBlock(handler.handlerBlockIdx)
      pred += (handlerBlock -> (pred.getOrElse(handlerBlock, Set()) + block))
    }

    (succ, pred)
  }

  private val exceptionHandlerBlocks: Set[Block] =
    (for (block <- blocks; handler <- block.exceptionHandlers) yield handler.handlerBlockIdx).toSet.map(getBlock)


  def getBlockIdx(block: Block) = blocks.indexOf(block)

  def getBlock(blockIdx: Int) = blocks(blockIdx)

  /**
    * successor blocks, not including exception handlers
    */
  def getSuccessors(block: Block): Set[Block] = succ(block)

  def getSuccessorsAndExceptionHandlers(block: Block): Set[Block] = getSuccessors(block) ++ getExceptionHandlers(block).map(_._2)

  /**
    * predecessor blocks, including exception edges
    */
  def getPredecessors(block: Block): Set[Block] = pred(block)

  def getExceptionHandlers(block: Block): Seq[(String, Block)] =
    block.exceptionHandlers.map(h => (h.exceptionType, getBlock(h.handlerBlockIdx)))

  /**
    * returns the next block (unless this is the last block), and in case of a conditional
    * jump, the conditional next block
    */
  def getJumpTargets(block: Block): (Option[Block], Option[Block]) = {
    val nextBlock = getNextBlock(block)
    val lastInstr = block.instr.last
    val jumpInstr = lastInstr.getJumpInstr
    if (jumpInstr.isDefined) {
      val succ = jumpInstr.get.getSuccessor()
      (if (succ._1.isDefined) succ._1.map(getBlock) else nextBlock, succ._2.map(getBlock))
    } else (nextBlock, None)
  }


  /**
    * returns the next block in the CFG (not necessarily a successor)
    */
  def getNextBlock(block: Block): Option[Block] = {
    val blockIdx = blocks.indexOf(block)
    if (blockIdx == blocks.size - 1)
      None
    else
      Some(blocks(blockIdx + 1))
  }

  def getNextNonExceptionBlock(block: Block): Option[Block] = {
    val next = getNextBlock(block)
    if (next.isDefined)
      if (isExceptionHandlerBlock(next.get)) getNextNonExceptionBlock(next.get)
      else next
    else None
  }

  def isExceptionHandlerBlock(block: Block): Boolean = exceptionHandlerBlocks contains block

  def getLastBlock(): Block = blocks.last
}

/**
  * group blocks depending on variational edges
  */
trait VBlockAnalysis extends CFGAnalysis {

  case class VBlock(firstBlock: Block, allBlocks: Set[Block])

  def isVariationalJump(fromBlock: Block, toBlock: Block): Boolean

  /**
    * VBlocks are a group of blocks that share a common context, identified
    * by the initial block to that group (the only block in this vblock that
    * blocks from other vblocks can jump to)
    *
    * The list is sorted, following the original order of the entry points
    * (which may not necessarily be very meaningful)
    */
  val vblocks: List[VBlock] = {
    //initially every block has its own id
    var vblockId: Map[Block, Int] = (blocks zip blocks.indices).toMap
    //which block is the first for each VBlock group?
    var vblockHead: Map[Int, Block] = (blocks.indices zip blocks).toMap

    var queue: Queue[Block] = Queue()
    queue = queue.enqueue(blocks)
    //analysis:
    //if all predecessors have the same id, and none of the edges is variational,
    //use the same id as parent
    while (queue.nonEmpty) {
      val (block, q) = queue.dequeue
      queue = q

      val pred = getPredecessors(block)
      val variationalEdgeFromPred = pred.exists(isVariationalJump(_, block))
      if (!variationalEdgeFromPred) {
        val predVBlockIdxs = pred.map(vblockId)
        val thisVBlockIdx = vblockId(block)
        if (predVBlockIdxs.size == 1 && predVBlockIdxs.head != thisVBlockIdx) {
          vblockId += (block -> predVBlockIdxs.head)
          queue = queue.enqueue(getSuccessorsAndExceptionHandlers(block))
        }
      }
    }
    for ((vblockId, blocks) <- vblockId.groupBy(_._2).toList.sortBy(_._1))
      yield VBlock(vblockHead(vblockId), blocks.keys.toSet)
  }


  def getNextVBlock(vblock: VBlock): Option[VBlock] = {
    assert(vblocks.contains(vblock), s"parameter $vblock is not a vblock")
    val b = vblocks.dropWhile(_ != vblock)
    if (b.isEmpty)
      None
    else b.tail.headOption
  }

  /**
    * is the given block the start of a VBlock?
    */
  def isVBlockHead(block: Block): Boolean = vblocks.exists(_.firstBlock == block)

  /**
    * are the successors of the given block in a different VBlock?
    */
  def isVBlockEnd(block: Block): Boolean = {
    assert(vblocks.filter(_.allBlocks contains block).size == 1, s"block $block not mapped to unique VBlock")
    val currentVBlock = vblocks.find(_.allBlocks contains block).get

    val succ = getSuccessors(block)

    assert(succ.forall(currentVBlock.allBlocks contains _) || succ.forall(s => !(currentVBlock.allBlocks contains s)), "either all or none of the successors should be in the current VBlock")

    !succ.forall(currentVBlock.allBlocks contains _)
  }

  /**
    * returns whether the first block is before the second block
    * in the current method
    */
  def isVBlockBefore(firstv: VBlock, secondv: VBlock): Boolean =
    vblocks.indexOf(firstv) < vblocks.indexOf(secondv)


  /**
    * finds the VBlock that this block is in
    */
  def getVBlock(block: Block): VBlock =
    vblocks.find(_.allBlocks contains block).get


  /**
    * return the VBlock that contains the return instruction(s)
    *
    * there should only be a single vblock with return instructions, because all
    * cases that allow multiple return/throw instructions require that all those
    * instructions are in method's original context
    */
  def getLastVBlock(): VBlock = {
    val vblocksWithReturn = vblocks.filter(_.allBlocks.exists(_.instr.exists(_.isReturnInstr)))
    assert(vblocksWithReturn.size == 1, "there should only be a single vblock with returns, but found " + vblocksWithReturn)
    vblocksWithReturn.head
  }

  def getVJumpTargets(block: Block): (Option[VBlock], Option[VBlock]) = {
    val jumpTargets = getJumpTargets(block)
    assert(jumpTargets._1.forall(isVBlockHead) && jumpTargets._2.forall(isVBlockHead),
      "all successors in a variational jump should be the heads of VBlocks")
    (jumpTargets._1.map(getVBlock), jumpTargets._2.map(getVBlock))
  }

  /**
    * get all exception handlers that may affect any block in this VBlock
    */
  def getVExceptionHandlers(vblock: VBlock): Set[(String, VBlock)] =
    vblock.allBlocks.flatMap(getExceptionHandlers).map(e => (e._1, getVBlock(e._2)))


  //sanity checks
  for (vblock <- vblocks)
    assert(vblock.allBlocks contains vblock.firstBlock, "VBlock: firstblock not in allblocks " + vblock)

}