package edu.cmu.cs.vbc.vbytecode

import org.objectweb.asm.{Label, Type}

class MethodEnv(val clazz: VBCClassNode, val method: VBCMethodNode) {
  protected val blocks = method.body.blocks
  assert(blocks.nonEmpty, "method with empty body not supported")

  //find all local variables
  protected val localVars: List[LocalVar] =
    (for (block <- blocks; instr <- block.instr; v <- instr.getVariables) yield v).distinct

  protected var labels: List[Label] = Nil
  //local variables refer to variables that are used in the byte code,
  //not to variables that are generated in the tranformation process;
  //the latter are stored separately as freshVars
  //(localVars and freshVars behave as sorted sets)
  protected var parameterCount = Type.getArgumentTypes(method.desc).size + (if (method.isStatic) 0 else 1)
  protected var freshVars: List[LocalVar] = Nil

  def getFreshVars(): List[Variable] = freshVars

  protected var blockLabels: Map[Block, Label] = Map()

  val thisParameter: Parameter = new Parameter(0)

  def freshLabel(name: String = "<unknown>") = {
    val l = new Label(name)
    labels ::= l
    l
  }

  def freshLocalVar() = {
    val l = new LocalVar()
    freshVars ::= l
    l
  }

  def getVarIdx(variable: Variable): Int = variable match {
    case p: Parameter =>
      assert(p.idx >= 0, "parameter with negative index not supported")
      p.idx
    case l: LocalVar =>
      val localIdxPos = localVars.reverse.indexOf(l)
      if (localIdxPos >= 0)
        parameterCount + localIdxPos
      else {
        val freshIdxPos = freshVars.reverse.indexOf(l)
        assert(freshIdxPos >= 0, "variable not found in environment")
        parameterCount + localVars.size + freshIdxPos
      }
  }


  def getBlock(blockIdx: Int) = blocks(blockIdx)

  def getLastBlock(): Block = blocks.last

  /**
    * returns a label for a block
    */
  def getBlockLabel(block: Block): Label = {
    if (!(blockLabels contains block))
      blockLabels += (block -> freshLabel("L" + blocks.indexOf(block)))
    blockLabels(block)
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

  /**
    * returns the next block (unless this is the last block), and in case of a conditional
    * jump, the conditional next block
    */
  def getSuccessors(block: Block): (Option[Block], Option[Block]) = {
    val nextBlock = getNextBlock(block)
    val lastInstr = block.instr.last
    val jumpInstr = lastInstr.getJumpInstr
    if (jumpInstr.isDefined) {
      val succ = jumpInstr.get.getSuccessor()
      (if (succ._1.isDefined) succ._1.map(getBlock) else nextBlock, succ._2.map(getBlock))
    } else (nextBlock, None)
  }

  def getPredecessors(thisBlock: Block): Set[Block] =
    for (block: Block <- blocks.toSet;
         succ = getSuccessors(block)
         if succ._1 == Some(thisBlock) || succ._2 == Some(thisBlock))
      yield block


  /**
    * returns whether the first block is before the second block
    * in the current method
    */
  def isBlockBefore(first: Block, second: Block): Boolean =
    blocks.indexOf(first) < blocks.indexOf(second)


  def isMain = {
    //todo oversimplified; also I'd rather create a new main method that just calls the lifted main method,
    //such that the (lifted) main method is not different from any other (lifted) method
    method.name == "main"
  }

  def maxLocals = parameterCount + localVars.size + freshVars.size

  val instructions = for (b <- blocks; i <- b.instr) yield i

  def getBlockStart(blockIdx: Int): Int = {
    var sum = 0
    blocks.take(blockIdx).foreach(sum += _.instr.length)
    sum
  }

}

