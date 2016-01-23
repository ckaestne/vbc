package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.Label

class MethodEnv(method: MethodNode) {
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
    protected var parameters: Set[Parameter] = Set()
    protected var maxParameterIdx = 0
    protected var freshVars: List[LocalVar] = Nil

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
                maxParameterIdx + 1 + localIdxPos
            else {
                val freshIdxPos = freshVars.reverse.indexOf(l)
                assert(freshIdxPos >= 0, "variable not found in environment")
                maxParameterIdx + 1 + localVars.size + freshIdxPos
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


    def addParameter(p: Parameter): Unit = {
        assert(!parameters.exists(_.idx == p.idx), "parameter " + p.idx + " already in environment")
        parameters += p
        maxParameterIdx = Math.max(maxParameterIdx, p.idx)
    }
}

/**
  * environment used during generation of the byte code and variational
  * byte code
  */
class VMethodEnv(method: MethodNode) extends MethodEnv(method) {


    var blockVars: Map[Block, Variable] = Map()



    val ctxParameter: Parameter = new Parameter(-1)


    def setBlockVar(block: Block, avar: Variable): Unit =
        blockVars += (block -> avar)

    def getBlockVar(block: Block): Variable = {
        if (!(blockVars contains block))
            blockVars += (block -> freshLocalVar())
        blockVars(block)
    }

    def getBlockVarVIdx(block: Block): Int = getVarIdx(getBlockVar(block))

    def getLocalVariables() = localVars




    /**
      * all values shifted by 1 by the ctx parameter
      */
    override def getVarIdx(variable: Variable): Int =
        if (variable eq ctxParameter) 1
        else {
            val idx = super.getVarIdx(variable: Variable)
            if (idx > 0) idx + 1
            else idx
        }


}
