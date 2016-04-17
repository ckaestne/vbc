package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc.analysis.{REF_TYPE, VBCAnalyzer, VBCFrame}
import edu.cmu.cs.vbc.utils.{LiftUtils, Statistics}
import edu.cmu.cs.vbc.vbytecode.instructions._

/**
  * Environment used during generation of the byte code and variational
  * byte code
  *
  * While initialized, VMethodEnv runs the stack analysis (@see [[VBCAnalyzer]]) and collect
  * essential information to:
  *
  * 1. decide whether or not to lift each instruction (see tagV section)
  * 1. handle unbalanced stack (see unbalanced stack section)
  */
class VMethodEnv(clazz: VBCClassNode, method: VBCMethodNode) extends MethodEnv(clazz, method) with VBlockAnalysis {

  val ctxParameter: Parameter = new Parameter(-1, "ctx")

  //////////////////////////////////////////////////
  // tagV
  //////////////////////////////////////////////////

  val blockTags = new Array[Boolean](blocks.length)

  // by default all elements are false

  /**
    * For each instruction, mark whether or not we need to lift it
    *
    * However, different instructions have different needs for tags. For example, for GETFIELD,
    * since we assume that all fields are Vs, the only concern is whether GETFIELD is performed on
    * a V. But for INVOKESPECIAL, we need more information, such as whether or not wrapping
    * method return value into a V, whether current method is invoked on a V, etc.
    *
    * Using an Int to represent instruction tag, we could have 32 different tags.
    */
  val instructionTags = new Array[Int](instructions.length)
  /**
    * Lift or not lift? Lifting means differently for different instructions.
    */
  val TAG_LIFT = 1
  /**
    * Method instructions only, whether we are having V arguments
    *
    * For some library methods that could not be lifted (e.g. java/lang/xxx), we try to invoke methods
    * with non-V arguments. But there might be cases where having V arguments is unavoidable (e.g. print
    * the value stored in local variable). For those cases, we use this tag to switch to model classes.
    */
  val TAG_HAS_VARG = 2
  /**
    * Method instructions only, whether we need to wrap return value into a V.
    *
    * We could always wrap all the return values into Vs, but that is not efficient and hard to debug from
    * reading bytecode.
    */
  val TAG_NEED_V_RETURN = 4
  /**
    * INVOKESPECIAL only. Handle special case like NEW DUP INVOKESPECIAL sequence.
    */
  val TAG_WRAP_DUPLICATE = 8

  def setTag(instr: Instruction, tag: Int): Unit = {
    assert(tag == TAG_LIFT || tag == TAG_HAS_VARG || tag == TAG_NEED_V_RETURN || tag == TAG_WRAP_DUPLICATE)
    instructionTags(getInsnIdx(instr)) |= tag
  }

  def getTag(instr: Instruction, tag: Int): Boolean = {
    assert(tag == TAG_LIFT || tag == TAG_HAS_VARG || tag == TAG_NEED_V_RETURN || tag == TAG_WRAP_DUPLICATE)
    (instructionTags(getInsnIdx(instr)) & tag) != 0
  }

  // by default all elements are false
  def getInsnIdx(instr: Instruction) = instructions.indexWhere(_ eq instr)

  def shouldLiftInstr(instr: Instruction): Boolean = (instructionTags(getInsnIdx(instr)) & TAG_LIFT) != 0

  def getOrderedSuccessorsIndexes(b: Block): List[Int] = {
    var idxSet = Set[Int]()
    var visited = Set[Block]()
    val succ = getSuccessors(b)
    var queue = List[Block]()
    queue = queue ++ succ
    while (queue.nonEmpty) {
      val h = queue.head
      visited += h
      if (!idxSet.contains(getBlockIdx(h))) {
        idxSet += getBlockIdx(h)
        val succ = getSuccessors(h)
        queue = queue ++ succ
      }
      queue = queue.tail
    }
    idxSet.toList sortWith (_ < _)
  }

  def getMergePoint(b1: Block, b2: Block): Int = {
    val l1: List[Int] = getOrderedSuccessorsIndexes(b1)
    val l2: List[Int] = getOrderedSuccessorsIndexes(b2)
    l1.find(l2.contains(_)).get
  }

  def setLift(instr: Instruction): Unit = {
    instructionTags(getInsnIdx(instr)) |= TAG_LIFT
  }

  def isBlockUnderCtx(i: Instruction): Boolean = {
    val blockIdx = blocks.indexWhere((bb) => bb.instr.exists((insn) => insn eq i))
    blockTags(blockIdx)
  }

  def getBlockForInstruction(i: Instruction): Block = blocks.find(_.instr contains i).get

  def isNonStaticL0(variable: Variable): Boolean = {
    if (method.isStatic) false
    else getVarIdxNoCtx(variable) == 0
  }

  /**
    * determines whether a CFJ edge between two blocks could be executed
    * in a stricter context than the context in which `fromBlock` was executed.
    *
    * This is the case when the last instruction of the `fromBlock` is
    * a variational jump (eg if condition on V value) or a method call that
    * could throw a variational exception. In contrast, GOTO or a normal
    * exception is a nonvariational jump and the `toBlock` will continue
    * in the same condition as the `fromBlock`
    */
  def isVariationalJump(fromBlock: Block, toBlock: Block): Boolean = {
    //TODO this should be informed by results of the tagV analysis
    //for now it's simply returning true for all IF statements and method
    //invocations

    val lastInstr = fromBlock.instr.last
    val fromBlockIdx = getBlockIdx(fromBlock)
    val toBlockIdx = getBlockIdx(toBlock)

    lastInstr match {
      case InstrGOTO(t) => false // GOTO does not change the context
      case method: MethodInstruction => true // all methods can have conditional exceptions
      case jump: JumpInstruction =>
        // all possible jump targets are variational, exception edges are not
        val succ = jump.getSuccessor()
        toBlockIdx == succ._1.getOrElse(fromBlockIdx + 1) || succ._2.exists(_ == toBlockIdx)
      case _ => false // exceptions do not change the context
    }
  }

  //////////////////////////////////////////////////
  // unbalanced stack
  //////////////////////////////////////////////////

  val analyzer = new VBCAnalyzer(this)
  val framesBefore: Array[VBCFrame] = analyzer.computeBeforeFrames
  val framesAfter: Array[VBCFrame] = analyzer.computeAfterFrames(framesBefore)
  val expectingVars: Map[Block, List[Variable]] = blocks.map(computeExpectingVars).toMap

  def getLeftVars(block: Block): List[Set[Variable]] = {
    val afterFrame = framesAfter(getInsnIdx(block.instr.last))
    val (succ1, succ2) = getJumpTargets(block)
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

  def getExpectingVars(block: Block): List[Variable] = expectingVars(block)

  def computeExpectingVars(block: Block): (Block, List[Variable]) = {
    val beforeFrame = framesBefore(getInsnIdx(block.instr.head))
    //ignore exceptions in handler blocks
    val stack = beforeFrame.stack.filterNot(_._1 == REF_TYPE(true))
    val newVars: Seq[Variable] =
      for (i <- 0 until stack.size)
        yield freshLocalVar("$unbalancedstack_b" + getBlockIdx(block) + "_v" + i, LiftUtils.vclasstype, LocalVar.initOneNull)
    (block, newVars.toList)
  }

  //////////////////////////////////////////////////
  // Block management
  //////////////////////////////////////////////////

  // allocate a variable for each VBlock, except for the first, which can reuse the parameter slot
  val vblockVars: Map[Block, Variable] =
    (for (((block, _), vblockidx) <- (vblocks zip vblocks.indices).tail) yield
      (block -> freshLocalVar("$blockctx" + vblockidx, LiftUtils.fexprclasstype, LocalVar.initFalse))).toMap +
      (vblocks.head._1 -> ctxParameter)

  def getVBlockVar(block: Block): Variable = {
    val vblock = vblocks.filter(_._2 contains block)
    assert(vblock.size == 1, "expected the block to be in exactly one VBlock")
    vblockVars(vblock.head._1)
  }

  //  def getVBlockVarVIdx(block: Block): Int = getVarIdx(getBlockVar(block))

  //////////////////////////////////////////////////
  // Utilities
  //////////////////////////////////////////////////


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
  // Statistics && debugging
  //////////////////////////////////////////////////
  Statistics.collectLiftingRatio(method.name, instructionTags.count(_ != 0), instructionTags.length)


  /** graphviz graph for debugging purposes */
  def toDot: String = {
    def blockname(b: Block) = "\"B" + getBlockIdx(b) + "\""
    def blocklabel(b: Block) = "B" + getBlockIdx(b) + ": v" + vblocks.indexWhere(_._1 == getVBlock(b)) + "\\n" +
      getExpectingVars(b).mkString("stack_load ", ", ", "\\n") +
      b.instr.mkString("\\n") + "\\n" +
      getLeftVars(b).mkString("stack_store ", ", ", "\\n")

    var result = "digraph G {\n"
    for (b <- blocks)
      result += s"  ${blockname(b)} [ shape=box label = " + "\"" + blocklabel(b) + "\"];\n"
    for (b <- blocks;
         succ <- getSuccessors(b))
      result += s"  ${blockname(b)} -> ${blockname(succ)}" +
        (if (isVariationalJump(b, succ)) "[ color=\"red\" ]" else "") + ";\n"

    result + "}"
  }

  // invariants to check correct working of unbalanced stack
  for (block <- blocks;
       succ <- getSuccessors(block);
       if getVBlock(block) != getVBlock(succ))
    assert(getExpectingVars(succ).size == getLeftVars(block).size, s"unbalanced stack mismatch: leaving ${getLeftVars(block)} in block ${getBlockIdx(block)} but expecting ${getExpectingVars(succ)} in block ${getBlockIdx(succ)}")


}
