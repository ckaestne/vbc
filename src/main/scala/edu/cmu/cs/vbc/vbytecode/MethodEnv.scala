package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc.analysis.VBCAnalyzer
import edu.cmu.cs.vbc.utils.Statistics
import edu.cmu.cs.vbc.vbytecode.instructions.{InstrINVOKESPECIAL, Instruction}
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
  protected var parameterCount = Type.getArgumentTypes(method.desc).size + (if (method.isStatic()) 0 else 1)
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

/**
  * environment used during generation of the byte code and variational
  * byte code
  */
class VMethodEnv(clazz: VBCClassNode, method: VBCMethodNode) extends MethodEnv(clazz, method) {

  ////////// Tag each instruction with V //////////

  val blockTags = new Array[Boolean](blocks.length)

  // by default all elements are false
  def getBlockIdx(b: Block) = blocks.indexWhere(_ eq b)

  /**
    * For each instruction, mark whether or not we need to lift it
    *
    * However, lifting means differently for different kinds of instructions. For example, for GETFIELD and PUTFIELD,
    * lifting means operating on V objects, thus we need invokedynamic.
    */
  val instructionTags = new Array[Boolean](instructions.length)

  // by default all elements are false
  def getInsnIdx(instr: Instruction) = instructions.indexWhere(_ eq instr)

  def shouldLiftInstr(instr: Instruction) = instructionTags(getInsnIdx(instr))

  def getOrderedSuccessorsIndexes(b: Block): List[Int] = {
    var idxSet = Set[Int]()
    var visited = Set[Block]()
    val (uncond, cond) = getSuccessors(b)
    var queue = List[Block]()
    if (uncond.isDefined) queue = queue :+ uncond.get
    if (cond.isDefined) queue = queue :+ cond.get
    while (queue.nonEmpty) {
      val h = queue.head
      visited += h
      if (!idxSet.contains(getBlockIdx(h))) {
        idxSet += getBlockIdx(h)
        val (o1, o2) = getSuccessors(h)
        if (o1.isDefined && !visited.contains(o1.get))
          queue = queue :+ o1.get
        if (o2.isDefined && !visited.contains(o2.get))
          queue = queue :+ o2.get
      }
      queue = queue.drop(1)
    }
    idxSet.toList sortWith (_ < _)
  }

  def getMergePoint(b1: Block, b2: Block): Int = {
    val l1: List[Int] = getOrderedSuccessorsIndexes(b1)
    val l2: List[Int] = getOrderedSuccessorsIndexes(b2)
    l1.find(l2.contains(_)).get
  }

  def setChainToTrue(chain: List[Instruction]): Unit = {
    for (i <- chain) instructionTags(getInsnIdx(i)) = true
  }

  def setInsnToTrue(instr: Instruction): Unit = {
    instructionTags(getInsnIdx(instr)) = true
  }

  def setInsnToFalse(instr: Instruction): Unit = {
    instructionTags(getInsnIdx(instr)) = false
  }

  def isBlockUnderCtx(i: Instruction): Boolean = {
    val blockIdx = blocks.indexWhere((bb) => bb.instr.exists((insn) => insn eq i))
    blockTags(blockIdx)
  }

  /**
    * Special hacking for cases like NEW DUP INVOKESPECIAL sequence, where careful wrapping is required
    */
  var needsWrappingInstrs: Set[Instruction] = Set()

  def addToWrappingInstrs(i: Instruction) = {
    assert(i.isInstanceOf[InstrINVOKESPECIAL], "Not an INVOKESPECIAL instruction: " + i)
    needsWrappingInstrs += i
  }

  def needsWrapping(i: Instruction): Boolean = needsWrappingInstrs.contains(i)

  def isL0(variable: Variable): Boolean = {
    if (method.isStatic()) false
    else getVarIdxNoCtx(variable) == 0
  }

  ////////// Unbalanced Stack //////////
  val analyzer = new VBCAnalyzer(this)
  assert(analyzer.beforeFrames.isDefined, "No frames available")
  assert(analyzer.afterFrames.isDefined, "No frames available")
  val framesBefore = analyzer.beforeFrames.get
  val framesAfter = analyzer.afterFrames.get
  var expectingVars: Map[Block, List[Variable]] = Map()
  blocks.foreach(getExpectingVars(_))

  def getLeftVars(block: Block): List[Set[Variable]] = {
    val afterFrame = framesAfter(getFrameIdx(block.instr.last))
    val (succ1, succ2) = getSuccessors(block)
    getVarSetList(Nil, succ1, succ2, afterFrame.getStackSize)
  }

  def getVarSetList(l: List[Set[Variable]], succ1: Option[Block], succ2: Option[Block], n: Int): List[Set[Variable]] =
    if (n == 0) l else getVarSetList(getVarSet(succ1, succ2, n) ::: l, succ1, succ2, n - 1)

  def getVarSet(succ1: Option[Block], succ2: Option[Block], n: Int): List[Set[Variable]] = {
    var set = Set[Variable]()
    if (succ1.isDefined) {
      val exp1 = getExpectingVars(succ1.get)
      if (exp1.nonEmpty) {
        set = set + exp1(n - 1)
      }
    }
    if (succ2.isDefined) {
      val exp2 = getExpectingVars(succ2.get)
      if (exp2.nonEmpty) {
        set = set + exp2(n - 1)
      }
    }
    List(set)
  }

  def getExpectingVars(block: Block): List[Variable] = {
    if (!(expectingVars contains block)) {
      val beforeFrame = framesBefore(getFrameIdx(block.instr.head))
      val newVars: List[Variable] = createNewVars(Nil, beforeFrame.getStackSize)
      expectingVars += (block -> newVars)
    }
    expectingVars(block)
  }

  def getFrameIdx(insn: Instruction): Int = instructions.indexWhere(_ eq insn)

  ////////// end Unbalanced Stack //////////

  var blockVars: Map[Block, Variable] = Map()


  def createNewVars(l: List[Variable], n: Int): List[Variable] =
    if (n == 0) l else createNewVars(List[Variable](freshLocalVar()) ::: l, n - 1)

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

  def isConditionalField(owner: String, name: String, desc: String): Boolean = {
    val filter = (f: VBCFieldNode) => {
      owner == clazz.name && name == f.name && f.desc == desc && f.hasConditionalAnnotation()
    }
    clazz.fields.exists(filter)
  }

  def getVarIdxNoCtx(variable: Variable) = super.getVarIdx(variable)

  /**
    * all values shifted by 1 by the ctx parameter
    */
  override def getVarIdx(variable: Variable): Int =
    if (variable eq ctxParameter) parameterCount
    else {
      val idx = super.getVarIdx(variable: Variable)
      if (idx >= parameterCount) idx + 1
      else idx
    }

  //////////////////////////////////////////////////
  // Statistics
  //////////////////////////////////////////////////
  Statistics.collectLiftingRatio(method.name, instructionTags.count(_ == true), instructionTags.length)

}
