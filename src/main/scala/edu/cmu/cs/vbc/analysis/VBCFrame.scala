package edu.cmu.cs.vbc.analysis

import edu.cmu.cs.vbc.test._
import edu.cmu.cs.vbc.utils.LiftingFilter
import edu.cmu.cs.vbc.vbytecode.VMethodEnv
import edu.cmu.cs.vbc.vbytecode.instructions._
import org.objectweb.asm.Type

/**
  * For each instruction, Frame contains information about local variables and stack elements.
  *
  * @param nLocals number of local variables of this frame
  */
class VBCFrame(nLocals: Int) {
  /**
    * Store values of local variables and stack elements
    */
  val values: Array[VBCValue] = new Array(nLocals * 10) // this is just an approximation

  /**
    * If value is updated, also store the previous instructions that update this value
    */
  val sourceInstrs: Array[Option[Instruction]] = new Array(values.length)

  /**
    * Whether the current frame is under context
    */
  //  var isInCtx: Boolean = false

  /**
    * The number of elements in the operand stack
    */
  var top = 0

  /**
    * The number of local variables
    */
  val locals = nLocals

  def this(srcFrame: VBCFrame) = {
    this(srcFrame.locals)
    for (i <- 0 until values.length) values(i) = srcFrame.values(i)
    for (i <- 0 until sourceInstrs.length) sourceInstrs(i) = srcFrame.sourceInstrs(i)
    top = srcFrame.top
    //    isInCtx = srcFrame.isInCtx
  }

  /**
    * Set the value for local variable with index i to value
    *
    * @param i     index
    * @param value new value to set to
    * @param instr previous instruction that sets this local variable
    */
  def setLocal(i: Int, value: VBCValue, instr: Option[Instruction]) = {
    values(i) = value
    sourceInstrs(i) = instr
  }

  /**
    * Merge two frames, for each variables, set the value to TOP if differs
    *
    * @param frame
    * @return
    */
  def merge(frame: VBCFrame): Boolean = {
    if (top != frame.top) {
      throw new RuntimeException("Incompatible stack heights")
    }
    var changed: Boolean = false
    for (i <- 0 until nLocals + top) {
      val merged = VBCValue.merge(values(i), frame.values(i))
      changed = merged != values(i) | changed
      if (changed) {
        values(i) = merged
        sourceInstrs(i) = frame.sourceInstrs(i)
      }
    }
    //    changed = changed | isInCtx != frame.isInCtx
    changed
  }

  /**
    * Execute the instruction corresponding to this frame
    *
    * @param instr instruction to be executed
    * @param env   A global env which contains all kinds of information about the current method
    * @return If not None, we need to backtrack because we finally realise we need to lift that instruction.
    *         By default every backtracked instruction should be lifted, except for GETFIELD, PUTFIELD, INVOKEVIRTUAL,
    *         INVOKESPECIAL, because lifting them or not depends on the type of object currently on stack. If the object
    *         is a V, we need to lift these instructions with INVOKEDYNAMIC.
    * @note NEW instruction requires special attention. Internally, NEW instruction puts the UNINITIALIZED type on stack,
    *       which is not subtype of java.lang.Object, and thus we could not wrap it into a V right after NEW. The
    *       workaround is to introduce a special NEW_REF_VALUE type in our analysis. Whenever NEW_REF_VALUE type value
    *       is taken from stack, we wrap the stack value into a V from there, before using the value. Now, only PUTFIELD
    *       instruction could expect a NEW_REF_VALUE ƒrom stack.
    *
    */
  def execute(instr: Instruction, env: VMethodEnv): Option[Instruction] = {

    /**
      * push to the stack
      *
      * @param v
      */
    def push(v: VBCValue, instr: Option[Instruction]): Unit = {
      if (top + nLocals >= values.length)
        throw new IndexOutOfBoundsException("Insufficient maxium stack size")
      values(top + nLocals) = v
      sourceInstrs(top + nLocals) = instr
      top += 1
    }

    /**
      * pop one value from stack
      *
      * @return
      */
    def pop(): (VBCValue, Option[Instruction]) = {
      if (top == 0)
        throw new IndexOutOfBoundsException("Cannot pop operand off an empty stack.")
      top -= 1
      val ret = values(top + nLocals)
      val instr = sourceInstrs(top + nLocals)
      values(nLocals + top) = null
      sourceInstrs(nLocals + top) = null
      (ret, instr)
    }

    val isV = env.shouldLiftInstr(instr)
    instr match {
      case i: InstrACONST_NULL => {
        if (isV)
          push(V_TYPE(), Some(i))
        else
          push(VBCValue.newValue(Type.getObjectType("null")), Some(i))
        None
      }
      case i: InstrICONST => {
        if (isV)
          push(V_TYPE(), Some(i))
        else
          push(INT_TYPE(), Some(i))
        None
      }
      case i: InstrBIPUSH => {
        if (isV)
          push(V_TYPE(), Some(i))
        else
          push(INT_TYPE(), Some(i))
        None
      }
      case i: InstrSIPUSH => {
        if (isV)
          push(V_TYPE(), Some(i))
        else
          push(INT_TYPE(), Some(i))
        None
      }
      case i: InstrLDC => {
        if (isV) {
          push(V_TYPE(), Some(i))
        }
        else
          i.o match {
            case integer: java.lang.Integer => push(INT_TYPE(), Some(i))
            case string: java.lang.String => push(VBCValue.newValue(Type.getObjectType("java/lang/String")), Some(i))
            case _ => throw new RuntimeException("Incomplete support for LDC")
          }
        None
      }
      case i: InstrILOAD => {
        env.setInsnToTrue(i)
        push(V_TYPE(), Some(i))
        if (values(env.getVarIdxNoCtx(i.variable)) != V_TYPE())
          sourceInstrs(env.getVarIdxNoCtx(i.variable))
        else None
      }
      case i: InstrALOAD => {
        /*
         * This assumes that all local variables other than this parameter to be V.
         *
         * In the future, if STORE operations are optimized, this could also be optimized to avoid loading V and
         * save some instructions.
         */
        if (env.isL0(i.variable)) {
          push(REF_TYPE(), Some(i))
          None
        }
        else {
          env.setInsnToTrue(i)
          push(V_TYPE(), Some(i))
          if (values(env.getVarIdxNoCtx(i.variable)) != V_TYPE())
            sourceInstrs(env.getVarIdxNoCtx(i.variable))
          else None
        }
      }
      case i: InstrISTORE => {
        // Now we assume all blocks are executed under some ctx other than method ctx,
        // meaning that all local variables should be a V, and so all ISTORE instructions
        // should be lifted
        env.setInsnToTrue(i)
        val (value, prev) = pop()
        if (value == V_TYPE()) {
          setLocal(env.getVarIdxNoCtx(i.variable), V_TYPE(), Some(i)) //TODO: float and double
          None
        }
        else {
          prev
        }
      }
      case i: InstrASTORE => {
        // The same as ISTORE
        env.setInsnToTrue(i)
        val (value, prev) = pop()
        if (value == V_TYPE()) {
          setLocal(env.getVarIdxNoCtx(i.variable), V_TYPE(), Some(i)) //TODO: float and double
          None
        }
        else {
          prev
        }
      }
      case i: InstrPOP => pop(); None
      case i: InstrDUP => val (v, prev) = pop(); push(v, prev); push(v, prev); None
      case i: InstrIADD => {
        val (v1, prev1) = pop()
        val (v2, prev2) = pop()
        if (isV) {
          if (v1 != V_TYPE()) return prev1 // if one of the operands is not V_Type, backtrack
          if (v2 != V_TYPE()) return prev2
          push(V_TYPE(), Some(i))
          None
        }
        else {
          push(INT_TYPE(), Some(i))
          None
        }
      }
      case i: InstrISUB => {
        val (v1, prev1) = pop()
        val (v2, prev2) = pop()
        if (isV) {
          if (v1 != V_TYPE()) return prev1 // if one of the operands is not V_Type, backtrack
          if (v2 != V_TYPE()) return prev2
          push(V_TYPE(), Some(i))
          None
        }
        else {
          push(INT_TYPE(), Some(i))
          None
        }
      }
      case i: InstrIMUL => {
        val (v1, prev1) = pop()
        val (v2, prev2) = pop()
        if (isV) {
          if (v1 != V_TYPE()) return prev1 // if one of the operands is not V_Type, backtrack
          if (v2 != V_TYPE()) return prev2
          push(V_TYPE(), Some(i))
          None
        }
        else {
          push(INT_TYPE(), Some(i))
          None
        }
      }
      case i: InstrIDIV => {
        val (v1, prev1) = pop()
        val (v2, prev2) = pop()
        if (isV) {
          if (v1 != V_TYPE()) return prev1 // if one of the operands is not V_Type, backtrack
          if (v2 != V_TYPE()) return prev2
          push(V_TYPE(), Some(i))
          None
        }
        else {
          push(INT_TYPE(), Some(i))
          None
        }
      }
      case i: InstrIINC => {
        // Now we assume all blocks are executed under some ctx other than method ctx,
        // meaning that all local variables should be a V, and so IINC instructions
        // should be lifted
        env.setInsnToTrue(i)
        setLocal(env.getVarIdxNoCtx(i.variable), V_TYPE(), Some(i))
        None
      }
      case i: InstrIFEQ => {
        // For now, no instructions will backtrack to jump instructions
        val (v, prev) = pop()
        env.setInsnToTrue(i)
        if (v != V_TYPE()) prev else None
      }
      case i: InstrIFNE => {
        val (v, prev) = pop()
        env.setInsnToTrue(i)
        if (v != V_TYPE()) prev else None
      }
      case i: InstrIFGE => {
        val (v, prev) = pop()
        env.setInsnToTrue(i)
        if (v != V_TYPE()) prev else None
      }
      case i: InstrIFGT => {
        val (v, prev) = pop()
        env.setInsnToTrue(i)
        if (v != V_TYPE()) prev else None
      }
      case i: InstrIF_ICMPEQ => {
        env.setInsnToTrue(i)
        val (v1, prev1) = pop()
        val (v2, prev2) = pop()
        if (v1 != V_TYPE()) prev1
        else if (v2 != V_TYPE()) prev2
        else None
      }
      case i: InstrIF_ICMPNE => {
        env.setInsnToTrue(i)
        val (v1, prev1) = pop()
        val (v2, prev2) = pop()
        if (v1 != V_TYPE()) prev1
        else if (v2 != V_TYPE()) prev2
        else None
      }
      case i: InstrIF_ICMPLT => {
        env.setInsnToTrue(i)
        val (v1, prev1) = pop()
        val (v2, prev2) = pop()
        if (v1 != V_TYPE()) prev1
        else if (v2 != V_TYPE()) prev2
        else None
      }
      case i: InstrIF_ICMPGE => {
        env.setInsnToTrue(i)
        val (v1, prev1) = pop()
        val (v2, prev2) = pop()
        if (v1 != V_TYPE()) prev1
        else if (v2 != V_TYPE()) prev2
        else None
      }
      case i: InstrGOTO => env.setInsnToTrue(i); None // does not affect locals and stack
      case i: InstrIRETURN => {
        // assuming all methods return V
        env.setInsnToTrue(i)
        val (v, prev) = pop()
        if (v != V_TYPE()) return prev else None
      }
      case i: InstrARETURN => {
        // assuming all methods return V
        env.setInsnToTrue(i)
        val (v, prev) = pop()
        if (v != V_TYPE()) return prev else None
      }
      case i: InstrRETURN => None // do nothing
      case i: InstrGETSTATIC => {
        /*
         * We assume that all fields are V
         */
        if (LiftingFilter.shouldLiftField(i.owner, i.name, i.desc)) {
          push(V_TYPE(), Some(i))
          env.setInsnToTrue(i)
        }
        else
          push(VBCValue.newValue(Type.getType(i.desc)), Some(i))
        None
      }
      case i: InstrPUTSTATIC => {
        val (v, prev) = pop()
        env.setInsnToTrue(i)
        // Assuming all static fields are of V type
        if (v != V_TYPE()) return prev
        None
      }
      case i: InstrGETFIELD => {
        val (v, prev) = pop()
        /*
         * Lifting PUTFIELD or not depends solely on the object on stack.
         * By default VBCAnalyzer would set this instruction to true if backtracked, so here
         * we need to check the object again and avoid that
         */
        if (v == V_TYPE()) env.setInsnToTrue(i) else env.setInsnToFalse(i) // lifting GETFIELD or not depends on L0
        push(V_TYPE(), Some(i))
        None
      }
      case i: InstrPUTFIELD => {
        val (value, prev1) = pop()
        val (ref, prev2) = pop()
        if (value != V_TYPE()) return prev1
        /*
         * Lifting PUTFIELD or not depends solely on the object on stack.
         * By default VBCAnalyzer would set this instruction to true if backtracked, so here
         * we need to check the object again and avoid that
         */
        if (ref == V_TYPE()) env.setInsnToTrue(i) else env.setInsnToFalse(i)
        None
      }
      case i: InstrINVOKEVIRTUAL => {
        val shouldLift = LiftingFilter.shouldLiftMethod(i.owner, i.name, i.desc)
        for (j <- Type.getArgumentTypes(i.desc).indices) {
          val (vj, prevj) = pop()
          if (shouldLift && vj != V_TYPE()) return prevj
        }
        val (ref, prev) = pop() // L0
        /*
         * Lifting PUTFIELD or not depends solely on the object on stack.
         * By default VBCAnalyzer would set this instruction to true if backtracked, so here
         * we need to check the object again and avoid that
         */
        if (ref == V_TYPE()) env.setInsnToTrue(i) else env.setInsnToFalse(i)
        if (Type.getReturnType(i.desc) != Type.VOID_TYPE) {
          if (shouldLift)
            push(V_TYPE(), Some(i))
          else
            push(VBCValue.newValue(Type.getReturnType(i.desc)), Some(i))
        }
        None
      }
      case i: InstrINVOKESTATIC => {
        val shouldLift = LiftingFilter.shouldLiftMethod(i.owner, i.name, i.desc)
        for (j <- Type.getArgumentTypes(i.desc).indices) {
          val (vj, prevj) = pop()
          if (shouldLift && vj != V_TYPE()) return prevj
        }
        if (Type.getReturnType(i.desc) != Type.VOID_TYPE) {
          push(V_TYPE(), Some(i))
        }
        None
      }
      case i: InstrINVOKESPECIAL => {
        val shouldLift = LiftingFilter.shouldLiftMethod(i.owner, i.name, i.desc)
        for (j <- Type.getArgumentTypes(i.desc).indices) {
          val (vj, prevj) = pop()
          if (shouldLift && vj != V_TYPE()) return prevj
        }
        val (ref, prev) = pop() // L0
        /*
         * Lifting PUTFIELD or not depends solely on the object on stack.
         * By default VBCAnalyzer would set this instruction to true if backtracked, so here
         * we need to check the object again and avoid that
         */
        if (ref == V_TYPE()) env.setInsnToTrue(i) else env.setInsnToFalse(i)
        // Special handling for <init>:
        // Whenever we see a V_REF_TYPE reference, we know that the initialized object would be consumed later as
        // method arguments or field values, so we scan the stack and wrap it into a V
        if (ref.isInstanceOf[V_REF_TYPE] && i.name == "<init>") {
          env.addToWrappingInstrs(i)
          // we only expect one duplicate on stack, otherwise calling one createOne is not enough
          assert(values(nLocals + top - 1) == ref, "No duplicate UNINITIALIZED value on stack")
          val exists = values.drop(nLocals).take(top - 1).contains(ref)
          assert(!exists, "More than one UNINITIALIZED value on stack")
          val (ref2, prev2) = pop()
          push(V_TYPE(), prev2)
        }
        if (Type.getReturnType(i.desc) != Type.VOID_TYPE) {
          if (shouldLift)
            push(V_TYPE(), Some(i))
          else
            push(VBCValue.newValue(Type.getReturnType(i.desc)), Some(i))
        }
        None
      }
      case i: InstrNEW => {
        if (isV)
          push(V_REF_TYPE(VBCValue.nextID), Some(i))
        else
          push(VBCValue.newValue(Type.getObjectType(i.t)), Some(i))
        None
      }
      case i: InstrINIT_CONDITIONAL_FIELDS => None // does not change stack
      case i: InstrDBGIPrint => {
        val (value, prev) = pop()
        if (value != V_TYPE()) prev else None
      }
      case i: InstrLoadConfig => push(V_TYPE(), Some(i)); None
      case i: InstrDBGCtx => None // does not change stack
      case i: TraceInstr_ConfigInit => None // does not change stack
      case i: TraceInstr_S => None // does not change stack
      case i: TraceInstr_Print => None // does not change stack
      case i: TraceInstr_GetField => None // does not change stack
      case _ => throw new RuntimeException("Instruction " + instr + " is not supported yet")
    }
  }

  override def toString: String = {
    val sb = new StringBuilder
    //    if (isInCtx) sb.append("[x]") else sb.append("[ ]")
    values.take(nLocals).foreach(sb.append(_))
    sb.append(' ')
    values.drop(nLocals).foreach((v) => if (v != null) sb.append(v) else sb.append("."))
    sb.toString()
  }

  def getStackSize = top
}
