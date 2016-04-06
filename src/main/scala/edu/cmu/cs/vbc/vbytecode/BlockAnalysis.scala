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
    var pred: Map[Block, Set[Block]] = Map()
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
          assert(false, s"expected jump instruction as last instruction of a block, but found $lastInstr")
      }
      for (handler <- block.exceptionHandlers)
        next += getBlock(handler.handlerBlockIdx)

      succ += (block -> next)
      for (n <- next)
        pred += (n -> (pred.getOrElse(n, Set()) + block))
    }
    (succ, pred)
  }


  def getBlockIdx(block: Block) = blocks.indexOf(block)

  def getBlock(blockIdx: Int) = blocks(blockIdx)

  def getSuccessors(block: Block): Set[Block] = succ(block)

  def getPredecessors(block: Block): Set[Block] = pred(block)

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

  /**
    * returns whether the first block is before the second block
    * in the current method
    */
  def isBlockBefore(first: Block, second: Block): Boolean =
    blocks.indexOf(first) < blocks.indexOf(second)

  def getLastBlock(): Block = blocks.last
}

/**
  * group blocks depending on variational edges
  */
trait VBlockAnalysis extends CFGAnalysis {


  def isVariationalEdge(fromBlock: Block, toBlock: Block): Boolean

  /**
    * VBlocks are a group of blocks that share a common context, identified
    * by the initial block to that group (the only block in this vblock that
    * blocks from other vblocks can jump to)
    *
    * The list is sorted, following the original order of the entry points
    * (which may not necessarily be very meaningful)
    */
  val vblocks: List[(Block, Set[Block])] = {
    //initially every block has its own id
    var vblockId: Map[Block, Int] = (blocks zip blocks.indices).toMap
    //which block is the first for each VBlock group?
    var vblockEntry: Map[Int, Block] = (blocks.indices zip blocks).toMap

    var queue: Queue[Block] = Queue()
    queue = queue.enqueue(blocks)
    //analysis:
    //if all predecessors have the same id, and none of the edges is variational,
    //use the same id as parent
    while (queue.nonEmpty) {
      val (block, q) = queue.dequeue

      val pred = getPredecessors(block)
      val variationalEdgeFromPred = pred.exists(isVariationalEdge(_, block))
      if (!variationalEdgeFromPred) {
        val predVBlockIdxs = pred.map(vblockId)
        val thisVBlockIdx = vblockId(block)
        if (predVBlockIdxs.size == 1 && predVBlockIdxs.head != thisVBlockIdx) {
          vblockId += (block -> predVBlockIdxs.head)
          queue = queue.enqueue(getSuccessors(block))
        }
      }
      queue = q
    }
    for ((vblockId, blocks) <- vblockId.groupBy(_._2).toList.sortBy(_._1))
      yield (vblockEntry(vblockId) -> blocks.keys.toSet)

  }


  def getVBlockId(block: Block): Int = ???


}