package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.vbytecode.instructions.{Instruction, JumpInstruction}
import edu.cmu.cs.vbc.vbytecode.{Parameter, VMethodEnv}
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
      initialFrame.write(new Parameter(if (env.method.isStatic) argIdx else argIdx + 1), VBCType(args(argIdx)), null)
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


    updateFrameForInstr(0, initialFrame)
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

  //  def afterFrames: Option[Array[VBCFrame]] = ??? /*{
  //    if (beforeFrames.isDefined) {
  //      val frames: Array[VBCFrame] = new Array[VBCFrame](beforeFrames.get.length)
  //      for (i <- frames.indices)
  //        frames(i) = new VBCFrame(beforeFrames.get(i))
  //      (frames zip instructions).foreach((pair) => pair._1.execute(pair._2, env))
  //      Some(frames)
  //    }
  //    else
  //      None
  //  }*/
}

