package edu.cmu.cs.vbc.vbytecode

import org.objectweb.asm.{Label, MethodVisitor, Type}

class MethodEnv(val clazz: VBCClassNode, val method: VBCMethodNode) extends CFGAnalysis {
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

  def getFreshVars(): List[LocalVar] = freshVars

  protected var blockLabels: Map[Block, Label] = Map()

  val thisParameter: Parameter = new Parameter(0, "this")

  def freshLabel(name: String = "<unknown>") = {
    val l = new Label(name)
    labels ::= l
    l
  }

  def freshLocalVar(name: String, desc: String, init: (MethodVisitor, VMethodEnv, LocalVar) => Unit): LocalVar = {
    val l = new LocalVar(name, desc, init)
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



  /**
    * returns a label for a block
    */
  def getBlockLabel(block: Block): Label = {
    if (!(blockLabels contains block))
      blockLabels += (block -> freshLabel("L" + blocks.indexOf(block)))
    blockLabels(block)
  }


  //  /**
  //    * returns the next block (unless this is the last block), and in case of a conditional
  //    * jump, the conditional next block
  //    */
  //  def getSuccessors(block: Block): (Option[Block], Option[Block]) = {
  //    val nextBlock = getNextBlock(block)
  //    val lastInstr = block.instr.last
  //    val jumpInstr = lastInstr.getJumpInstr
  //    if (jumpInstr.isDefined) {
  //      val succ = jumpInstr.get.getSuccessor()
  //      (if (succ._1.isDefined) succ._1.map(getBlock) else nextBlock, succ._2.map(getBlock))
  //    } else (nextBlock, None)
  //  }
  //
  //  def getPredecessors(thisBlock: Block): Set[Block] =
  //    for (block: Block <- blocks.toSet;
  //         succ = getSuccessors(block)
  //         if succ._1.contains(thisBlock) || succ._2.contains(thisBlock))
  //      yield block




  def isMain = {
    //todo oversimplified
    method.name == "main"
  }

  def maxLocals = parameterCount + localVars.size + freshVars.size

  // get all instructions in sequence
  val instructions = for (b <- blocks; i <- b.instr) yield i
  // indices of first instructions in each block
  lazy val instructionBlockBoundaries = blocks.foldLeft(List(0))((l, block) => (l.head + block.instr.size) :: l).reverse

  // lookup the block for an instruction
  def blockForInstruction(instrIdx: Int): Block = {
    var idx = instrIdx
    for (b <- blocks) {
      idx -= b.instr.size
      if (idx < 0)
        return b
    }
    return null
  }

  def getBlockStart(blockIdx: Int): Int = {
    var sum = 0
    blocks.take(blockIdx).foreach(sum += _.instr.length)
    sum
  }

}

