package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.vbytecode.instructions.{Instruction, JumpInstruction}
import edu.cmu.cs.vbc.vbytecode.{Block, Parameter, VMethodEnv, Variable}
import org.objectweb.asm.Type

import scala.collection.mutable.Queue

/**
  * Compute frames before and after bytecode execution
  *
  * @author chupanw
  */
class VBCAnalyzer(env: VMethodEnv) {


  /**
    * An map of frames before executing each instruction
    */
  lazy val computeBeforeFrames: Map[Instruction, VBCFrame] = {
    // we don't compute frames for abstract or native methods
    if (env.method.isAbstract || env.method.isNative) {
      Map()
    }
    val instructions = env.instructions
    // init
    var beforeInstructionFrames: Map[Instruction, VBCFrame] = Map()
    val queue: Queue[Int] = Queue()
    var initialFrame: VBCFrame = new VBCFrame(Map(), Nil)
    var local: Int = 0
    // init this
    if (!env.method.isStatic) {
      initialFrame = initialFrame.write(env.thisParameter, VBCType(Type.getObjectType(env.clazz.name)), null)
    }
    // init args
    val args = Type.getArgumentTypes(env.method.desc)
    for (argIdx <- 0 until args.size) {
      initialFrame = initialFrame.write(new Parameter(if (env.method.isStatic) argIdx else argIdx + 1), VBCType(args(argIdx)), null)
    }

    def getInstr(insn: Int) = instructions(insn)

    /**
      * Merge the new frame into frames array
      */
    def updateFrameForInstr(insn: Int, frame: VBCFrame): VBCFrame = {
      val instr = getInstr(insn)
      val oldFrame = beforeInstructionFrames.get(instr)
      var changed = false
      var result: VBCFrame = frame
      if (!oldFrame.isDefined) {
        changed = true
      }
      else if (oldFrame.get != frame) {
        result = oldFrame.get.merge(frame)
        changed = true
      }

      if (changed && !(queue contains insn)) {
        queue.enqueue(insn)
      }
      beforeInstructionFrames += (instr -> result)
      result
    }

    beforeInstructionFrames += (getInstr(0) -> initialFrame)
    queue.enqueue(0)
    // control flow analysis
    while (queue.nonEmpty) {
      val insn = queue.dequeue
      val instr = getInstr(insn)
      val frame = instr.updateStack(beforeInstructionFrames(instr))
      instr match {
        case jump: JumpInstruction =>
          val (uncond, cond) = jump.getSuccessor()
          if (cond.isDefined) updateFrameForInstr(env.getBlockStart(cond.get), frame)
          if (uncond.isDefined)
            updateFrameForInstr(env.getBlockStart(uncond.get), frame)
          else
            updateFrameForInstr(insn + 1, frame)
        case i if !i.isReturnInstr =>
          updateFrameForInstr(insn + 1, frame)
        case _ =>
      }
    }
    beforeInstructionFrames
  }


  /**
    * Unbalanced stack:
    *
    * We compute for each block whether the block leaves
    * certain values on the stack or needs values from
    * the stack. We represent possible values and map them
    * with variables (one block's left value is represented
    * by the same value as the next block's expected value).
    *
    * This allows us to balance all stacks by storing all
    * left values in variables and loading all expected values
    * from variables instead of relying on the stack.
    *
    * It is the following block that defines which variables
    * it expects to read from, and the previous blocks have
    * to identify which variables store left values.
    *
    * This is a first sound, but not necessarily efficient solution.
    * TODO A better analysis will identify when values actually
    * need to be read within a block (if a value remains unmodified
    * on the stack it does not need to be read and stored).
    * TODO A better analysis will also avoid writing to multiple
    * variables in many cases.
    */

  /**
    * compute the expected and left lists for each block
    */
  def computeUnbalancedStack(): (Map[Block, List[Variable]], Map[Block, List[Set[Variable]]]) = {
    val blockExpectedVars: Map[Block, List[Variable]] =
      env.method.body.blocks.map(block => {
        val beforeBlockFrame = computeBeforeFrames(block.instr.head)
        val newVars: List[Variable] = (0 until beforeBlockFrame.stack.size).toList.map(a => env.freshLocalVar())
        (block -> newVars)
      }).toMap

    val blockLeftVars: Map[Block, List[Set[Variable]]] =
      env.method.body.blocks.map(block => {
        val lastInstr = block.instr.last
        val frameBefore = computeBeforeFrames(lastInstr)
        val frameAfter = lastInstr.updateStack(frameBefore)
        val (succ1, succ2) = env.getSuccessors(block)

        val expected1 = succ1.map(blockExpectedVars)
        val expected2 = succ2.map(blockExpectedVars)
        val leftVars: List[Set[Variable]] =
          if (expected1.isEmpty) expected2.getOrElse(Nil).map(Set(_))
          else if (expected2.isEmpty) expected1.get.map(Set(_))
          else expected1.get.zip(expected2.get).map(p => Set(p._1, p._2))
        (block -> leftVars)
      }).toMap

    (blockExpectedVars, blockLeftVars)
  }


}

