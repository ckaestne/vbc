package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc.analysis.{VBCAnalyzer, VBCFrame}
import edu.cmu.cs.vbc.utils.{LiftUtils, Statistics}
import edu.cmu.cs.vbc.vbytecode.instructions.Instruction

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
class VMethodEnv(clazz: VBCClassNode, method: VBCMethodNode) extends MethodEnv(clazz, method) {

  //////////////////////////////////////////////////
  // tagV
  //////////////////////////////////////////////////

  val blockTags = new Array[Boolean](blocks.length)

  // by default all elements are false
  def getBlockIdx(b: Block) = blocks.indexWhere(_ eq b)

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
    * Whether we need to wrap value into a V.
    *
    * We could always wrap all the return values into Vs, but that is not efficient and hard to debug from
    * reading bytecode.
    */
  val TAG_NEED_V = 4
  /**
    * INVOKESPECIAL only. Handle special case like NEW DUP INVOKESPECIAL sequence.
    */
  val TAG_WRAP_DUPLICATE = 8

  def setTag(instr: Instruction, tag: Int): Unit = {
    assert(tag == TAG_LIFT || tag == TAG_HAS_VARG || tag == TAG_NEED_V || tag == TAG_WRAP_DUPLICATE)
    instructionTags(getInsnIdx(instr)) |= tag
  }

  def getTag(instr: Instruction, tag: Int): Boolean = {
    assert(tag == TAG_LIFT || tag == TAG_HAS_VARG || tag == TAG_NEED_V || tag == TAG_WRAP_DUPLICATE)
    (instructionTags(getInsnIdx(instr)) & tag) != 0
  }

  // by default all elements are false
  def getInsnIdx(instr: Instruction) = instructions.indexWhere(_ eq instr)

  def shouldLiftInstr(instr: Instruction): Boolean = (instructionTags(getInsnIdx(instr)) & TAG_LIFT) != 0

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

  def setLift(instr: Instruction): Unit = {
    instructionTags(getInsnIdx(instr)) |= TAG_LIFT
  }

  def isBlockUnderCtx(i: Instruction): Boolean = {
    val blockIdx = blocks.indexWhere((bb) => bb.instr.exists((insn) => insn eq i))
    blockTags(blockIdx)
  }

  def isNonStaticL0(variable: Variable): Boolean = {
    if (method.isStatic) false
    else getVarIdxNoCtx(variable) == 0
  }

  //////////////////////////////////////////////////
  // unbalanced stack
  //////////////////////////////////////////////////

  val analyzer = new VBCAnalyzer(this)
  val framesBefore: Array[VBCFrame] = analyzer.computeBeforeFrames
  val framesAfter: Array[VBCFrame] = analyzer.computeAfterFrames(framesBefore)
  val expectingVars: Map[Block, List[Variable]] = blocks.map(computeExpectingVars(_)).toMap

  def getLeftVars(block: Block): List[Set[Variable]] = {
    val afterFrame = framesAfter(getInsnIdx(block.instr.last))
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

  def getExpectingVars(block: Block): List[Variable] = expectingVars(block)

  def computeExpectingVars(block: Block): (Block, List[Variable]) = {
    val beforeFrame = framesBefore(getInsnIdx(block.instr.head))
    val newVars: List[Variable] = createNewVars(Nil, beforeFrame.getStackSize)
    (block, newVars)
  }


  //////////////////////////////////////////////////
  // Utilities
  //////////////////////////////////////////////////

  var blockVars: Map[Block, Variable] = Map()

  def createNewVars(l: List[Variable], n: Int): List[Variable] =
    if (n == 0) l else createNewVars(List[Variable](freshLocalVar("$unbalancedstack" + n, LiftUtils.vclasstype)) ::: l, n - 1)

  val ctxParameter: Parameter = new Parameter(-1, "ctx", TypeDesc(LiftUtils.fexprclasstype))


  def setBlockVar(block: Block, avar: Variable): Unit =
    blockVars += (block -> avar)

  def getBlockVar(block: Block): Variable = {
    if (!(blockVars contains block))
      blockVars += (block -> freshLocalVar("$blockctx" + method.body.blocks.indexOf(block), LiftUtils.fexprclasstype))
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
  Statistics.collectLiftingRatio(method.name, instructionTags.count(_ != 0), instructionTags.length)

}
