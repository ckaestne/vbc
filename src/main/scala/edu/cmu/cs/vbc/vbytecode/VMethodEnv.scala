package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc.analysis.VBCAnalyzer
import edu.cmu.cs.vbc.utils.Statistics
import edu.cmu.cs.vbc.vbytecode.instructions.{InstrINVOKESPECIAL, Instruction}

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

  //////////////////////////////////////////////////
  // unbalanced stack
  //////////////////////////////////////////////////

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


  //////////////////////////////////////////////////
  // Utilities
  //////////////////////////////////////////////////

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
