package edu.cmu.cs.vbc.vbytecode.instructions

import edu.cmu.cs.vbc.OpcodePrint
import edu.cmu.cs.vbc.analysis.VBCFrame.{FrameEntry, UpdatedFrame}
import edu.cmu.cs.vbc.analysis._
import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.utils.{InvokeDynamicUtils, VCall}
import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassVisitor, Handle, MethodVisitor, Type}

/**
  * @author chupanw
  */

trait MethodInstruction extends Instruction {


//  def invokeDynamic(
//                     owner: Owner,
//                     name: MethodName,
//                     desc: MethodDesc,
//                     itf: Boolean,
//                     mv: MethodVisitor,
//                     env: VMethodEnv,
//                     defaultLoadCtx: MethodVisitor => Unit
//                   ): Unit = {
//    val nArgs = Type.getArgumentTypes(desc).length
//    val hasVArgs = nArgs > 0
//    val liftedCall = liftCall(owner, name, desc)
//    val objType = Type.getObjectType(liftedCall.owner).toString
//    val argTypeDesc: String = desc.getArgs.map {
//      t => if (liftedCall.isLifting) t.toV.desc else t.castInt.toObject.toModel.desc
//    }.mkString("(", "", ")")
//
//    val isReturnVoid = desc.isReturnVoid
//    val retType = if (isReturnVoid) "V" else vclasstype
//    val vCall = if (isReturnVoid) VCall.sforeach else VCall.sflatMap
//
//    val invokeType = getInvokeType
//    assert(invokeType != INVOKESTATIC)
//
//    InvokeDynamicUtils.invokeWithCacheClear(
//      vCall,
//      mv,
//      env,
//      defaultLoadCtx,
//      OpcodePrint.print(invokeType) + "$" + name.name,
//      s"$objType$argTypeDesc$retType",
//      nExplodeArgs = if (liftedCall.isLifting) 0 else desc.getArgCount,
//      expandArgArray = !liftedCall.isLifting && desc.getArgs.exists(_.isArray)
//    ) {
//      (mv: MethodVisitor) => {
//        if (!liftedCall.isLifting && hasVArgs) {
//          mv.visitVarInsn(ALOAD, 0) // objref
//          1 until nArgs foreach { (i) => loadVar(i, liftedCall.desc, i - 1, mv) } // first nArgs -1 arguments
//          loadVar(nArgs + 1, liftedCall.desc, nArgs - 1, mv) // last argument
//        } else {
//          mv.visitVarInsn(ALOAD, nArgs + 1) // objref
//          0 until nArgs foreach { (i) => loadVar(i, liftedCall.desc, i, mv) } // arguments
//        }
//        if (liftedCall.isLifting) mv.visitVarInsn(ALOAD, nArgs) // ctx
//        // would need to push nulls if invoking <init>, but this is strange
//        assert(name != MethodName("<init>"), "calling <init> on a V object")
//        interceptCalls(liftedCall, mv, nArgs, env) {
//          mv.visitMethodInsn(invokeType, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
//        }
//        if (shouldTransformReturnType(liftedCall)) {
//          // Box primitive type
//          boxReturnValue(liftedCall.desc, mv)
//          // perf: Necessary because we assume V[] everywhere.
//          toVArray(liftedCall, mv, nArgs)
//        }
//        if (!LiftingPolicy.shouldLiftMethodCall(owner, name, desc) && !isReturnVoid)
//          callVCreateOne(mv, (m) => m.visitVarInsn(ALOAD, nArgs))
//        //cpwtodo: when calling RETURN, there might be a V<Exception> on stack, but for now just ignore it.
//        if (isReturnVoid) mv.visitInsn(RETURN) else mv.visitInsn(ARETURN)
//      }
//    }
//  }
//
//  def shouldTransformReturnType(liftedCall: LiftedCall): Boolean = {
//    (liftedCall.owner, liftedCall.name, liftedCall.desc) match {
//      case (Owner("java/lang/Object"), MethodName("equals"), MethodDesc("(Ljava/lang/Object;)Z")) => false
//      case (Owner("java/lang/Object"), MethodName("hashCode"), MethodDesc("()I")) => false
//      case _ => true
//    }
//  }
//
//  def interceptCalls(call: LiftedCall, mv: MethodVisitor, ctxIdx: Int, env: VMethodEnv)(otherwise: => Unit): Unit = {
//    if (call.owner == Owner("java/io/PrintStream") && call.name.name == "println") {
//      mv.visitVarInsn(ALOAD, ctxIdx)  // load context
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.prependPrintStream.appendFE, false)
//    } else if (call.owner == Owner("java/lang/Class") && call.name.name == "newInstance") {
//      mv.visitVarInsn(ALOAD, ctxIdx)  // load context
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, s"(Ljava/lang/Class;$fexprclasstype)Ljava/lang/Object;", false)
//    } else if (call.owner == Owner("org/apache/commons/beanutils/BeanUtilsBean") && call.name.name == "copyProperty") {
//      mv.visitVarInsn(ALOAD, ctxIdx)  // load context
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.prepend(call.owner.getTypeDesc).appendFE, false)
//    } else if (call.owner == Owner("java/lang/reflect/Field") && call.name.name == "getType") {
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.prepend(call.owner.getTypeDesc), false)
//    } else if (call.owner == Owner("java/lang/reflect/Field") && call.name.name == "getInt") {
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.prepend(call.owner.getTypeDesc), false)
//    } else if (call.owner == Owner("java/lang/Class") && call.name.name == "getConstructor") {
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.prepend(call.owner.getTypeDesc), false)
//    } else if (call.owner == Owner("java/lang/reflect/Constructor") && call.name.name == "newInstance") {
//      mv.visitVarInsn(ALOAD, ctxIdx)  // load context
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.prepend(call.owner.getTypeDesc).appendFE, false)
//    } else if (call.owner == Owner("java/lang/Object") && call.name.name == "equals") {
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.toVs.prepend(call.owner.getTypeDesc).appendFE, false)
//    } else if (call.owner == Owner("java/lang/Object") && call.name.name == "hashCode") {
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.toVs.prepend(call.owner.getTypeDesc).appendFE, false)
//    } else if (call.owner == Owner("java/lang/Object") && call.name.name == "toString") {
//      // This could create infinite loop: if a class overrides toString() and calls super.toString() inside it
//      // To avoid the above scenario, we need to detect if the invoking object comes from ALOAD 0, if yes, we do not
//      // intercept this call.
//      // For now, we naively assume that the previous instruction is ALOAD 0
//      val thisIdx: Int = env.instructions.indexWhere(_ eq this)
//      assert(thisIdx != -1, "Could not find this instruction")
//      val previous: Instruction = env.instructions(thisIdx - 1)
//      if (previous.isALOAD0) {
//        mv.visitInsn(POP) // pop the useless ctx
//        otherwise
//      }
//      else
//        mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.toVs.prepend(call.owner.getTypeDesc).appendFE, false)
//    } else if (call.owner == Owner("java/lang/Class") && call.name.name == "getDeclaredMethod") {
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.prepend(call.owner.getTypeDesc), false)
//    } else if (call.owner == Owner("java/lang/reflect/Method") && call.name.name == "invoke") {
//      mv.visitVarInsn(ALOAD, ctxIdx)
//      mv.visitMethodInsn(INVOKESTATIC, Owner.getVOps, call.name, call.desc.prepend(call.owner.getTypeDesc).appendFE, false)
//    }
//    else
//      otherwise
//  }
//
//  def getInvokeType: Int = this match {
//    case i: InstrINVOKEVIRTUAL => INVOKEVIRTUAL
//    case i: InstrINVOKESPECIAL => INVOKESPECIAL
//    case i: InstrINVOKEINTERFACE => INVOKEINTERFACE
//    case i: InstrINVOKESTATIC => INVOKESTATIC
//    case _ => throw new UnsupportedOperationException("Unsupported invoke type")
//  }
//
//  def boxReturnValue(desc: MethodDesc, mv: MethodVisitor): Unit = {
//    desc.getReturnTypeSort match {
//      case Type.INT =>
//        mv.visitMethodInsn(INVOKESTATIC, Owner.getInt, MethodName("valueOf"), MethodDesc(s"(I)${TypeDesc.getInt}"), false)
//      case Type.BOOLEAN =>
//        mv.visitMethodInsn(INVOKESTATIC, Owner.getInt, MethodName("valueOf"), MethodDesc(s"(I)${TypeDesc.getInt}"), false)
//      case Type.LONG =>
//        mv.visitMethodInsn(INVOKESTATIC, Owner.getLong, MethodName("valueOf"), MethodDesc(s"(J)${TypeDesc.getLong}"), false)
//      case Type.DOUBLE =>
//        mv.visitMethodInsn(INVOKESTATIC, Owner.getDouble, MethodName("valueOf"), MethodDesc(s"(D)${TypeDesc.getDouble}"), false)
//      case Type.CHAR =>
//        mv.visitMethodInsn(INVOKESTATIC, Owner.getInt, MethodName("valueOf"), MethodDesc(s"(I)${TypeDesc.getInt}"), false)
//      case Type.FLOAT =>
//        mv.visitMethodInsn(INVOKESTATIC, Owner.getFloat, MethodName("valueOf"), MethodDesc(s"(F)${TypeDesc.getFloat}"), false)
//      case Type.OBJECT => // do nothing
//      case Type.VOID => // do nothing
//      case Type.ARRAY => // do nothing
//      case _ => ???
//    }
//  }
//
//  /**
//    * Turn primitive array to V array
//    */
//  def toVArray(lifted: LiftedCall, mv: MethodVisitor, ctxIdx: Int): Unit = {
//    lifted.desc.getReturnType match {
//      case Some(TypeDesc("[I")) => ???
//      case Some(TypeDesc("[C")) =>
//        mv.visitVarInsn(ALOAD, ctxIdx)
//        mv.visitMethodInsn(
//          INVOKESTATIC,
//          Owner.getArrayOps,
//          "CArray2VArray",
//          MethodDesc(s"([C$fexprclasstype)[$vclasstype"),
//          false
//        )
//      case Some(TypeDesc("[S")) => ???
//      case Some(TypeDesc("[Z")) => ???
//      case Some(TypeDesc("[B")) =>
//        mv.visitVarInsn(ALOAD, ctxIdx)
//        mv.visitMethodInsn(
//          INVOKESTATIC,
//          Owner.getArrayOps,
//          "BArray2VArray",
//          MethodDesc(s"([B$fexprclasstype)[$vclasstype"),
//          false
//        )
//      case Some(TypeDesc("[J")) => ???
//      case Some(TypeDesc("[F")) => ???
//      case Some(TypeDesc("[D")) => ???
//      case Some(t) if t.isArray =>
//        mv.visitVarInsn(ALOAD, ctxIdx)
//        mv.visitMethodInsn(
//          INVOKESTATIC,
//          Owner.getArrayOps,
//          "ObjectArray2VArray",
//          MethodDesc(s"([Ljava/lang/Object;$fexprclasstype)[$vclasstype"),
//          false
//        )
//      case _ =>
//        if (lifted.owner == Owner("java/lang/reflect/Array") && lifted.name == MethodName("newInstance")) {
//          mv.visitTypeInsn(CHECKCAST, "[Ljava/lang/Object;")
//          mv.visitVarInsn(ALOAD, ctxIdx)
//          mv.visitMethodInsn(
//            INVOKESTATIC,
//            Owner.getArrayOps,
//            "ObjectArray2VArray",
//            MethodDesc(s"([Ljava/lang/Object;$fexprclasstype)[$vclasstype"),
//            false
//          )
//        }
//    }
//  }
//
//  def loadVar(index: Int, desc: MethodDesc, indexInDesc: Int, mv: MethodVisitor): Unit = {
//    mv.visitVarInsn(ALOAD, index)
//    val args = desc.getArgs
//    args(indexInDesc) match {
//      case TypeDesc("Z") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getInt, MethodName("intValue"), MethodDesc("()I"), false)
//      case TypeDesc("C") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getInt, MethodName("intValue"), MethodDesc("()I"), false)
//      case TypeDesc("B") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getInt, MethodName("intValue"), MethodDesc("()I"), false)
//      case TypeDesc("S") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getInt, MethodName("intValue"), MethodDesc("()I"), false)
//      case TypeDesc("I") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getInt, MethodName("intValue"), MethodDesc("()I"), false)
//      case TypeDesc("F") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getFloat, MethodName("floatValue"), MethodDesc("()F"), false)
//      case TypeDesc("J") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getLong, MethodName("longValue"), MethodDesc("()J"), false)
//      case TypeDesc("D") => mv.visitMethodInsn(INVOKEVIRTUAL, Owner.getDouble, MethodName("doubleValue"), MethodDesc("()D"), false)
//      case _ => // nothing
//    }
//  }
//
//  /** Invoke methods on nonV object with V arguments
//    *
//    * [[edu.cmu.cs.vbc.analysis.VBCAnalyzer]] ensures that all V arguments will be Vs.
//    * Call a helper static method instead of making the original call. Inside the helper method,
//    * argument order will be tweaked so that we can call [[InvokeDynamicUtils.invoke()]]
//    */
//  def invokeOnNonV(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean, mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
//    val helperName = "helper$invokeOnNonV$" + env.clazz.lambdaMethods.size
//    val helperDesc: String =
//      "(" + owner.getTypeDesc.desc + vclasstype * desc.getArgCount + fexprclasstype + ")" +
//        (if (desc.isReturnVoid) "V" else vclasstype)
//    val helper = (cv: ClassVisitor) => {
//      val m: MethodVisitor = cv.visitMethod(
//        ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
//        helperName, // method name
//        helperDesc, // descriptor
//        helperDesc, // signature
//        Array[String]() // exception
//      )
//      m.visitCode()
//      m.visitVarInsn(ALOAD, 0) // objref
//      callVCreateOne(m, (m) => m.visitVarInsn(ALOAD, desc.getArgCount + 1))
//      1 to desc.getArgCount foreach { (i) => m.visitVarInsn(ALOAD, i) } // arguments
//      invokeDynamic(owner, name, desc, itf, m, env, defaultLoadCtx = (m) => m.visitVarInsn(ALOAD, desc.getArgCount + 1))
//      if (desc.isReturnVoid) m.visitInsn(RETURN) else m.visitInsn(ARETURN)
//      m.visitMaxs(10, 10)
//      m.visitEnd()
//    }
//    env.clazz.lambdaMethods += (helperName -> helper)
//    mv.visitMethodInsn(INVOKESTATIC, Owner(env.clazz.name), MethodName(helperName), MethodDesc(helperDesc), false)
//  }
//
//  override def doBacktrack(env: VMethodEnv): Unit = env.setTag(this, env.TAG_NEED_V)

  def updateStack(
                   s: VBCFrame,
                   env: VMethodEnv,
                   owner: Owner,
                   name: MethodName,
                   desc: MethodDesc
                 ): UpdatedFrame = {
//    val shouldLift = LiftingPolicy.shouldLiftMethodCall(owner, name, desc)
    val shouldLift = env.policy.liftCall(owner, name, desc)
    val nArg = Type.getArgumentTypes(desc).length
    val argList: List[(VBCType, Set[Instruction])] = s.stack.take(nArg)
    val hasVArgs = argList.exists(_._1.isInstanceOf[V_TYPE])
    val hasArrayArgs = /*CHK LiftingPolicy.liftCall(owner, name, desc).*/desc.getArgs.exists(_.isArray)

    // object reference
    var frame = s
    1 to nArg foreach { _ => frame = frame.pop()._3 }
    if (!this.isInstanceOf[InstrINVOKESTATIC]) {
      val (ref, ref_prev, baseFrame) = frame.pop() // L0
      frame = baseFrame
      if (ref == V_TYPE(false)) env.setLift(this)
      if (this.isInstanceOf[InstrINVOKESPECIAL] && name.contentEquals("<init>")) {
        if (ref.isInstanceOf[V_REF_TYPE]) {
          // We expect uninitialized references appear in pairs.
          // Whenever we see a V_REF_TYPE reference, we know that the initialized object would be consumed later as
          // method arguments or field values, so we scan the stack and wrap it into a V
          env.setTag(this, env.TAG_WRAP_DUPLICATE)
          // we only expect one duplicate on stack, otherwise calling one createOne is not enough
          assert(frame.stack.head._1 == ref, "No duplicate UNINITIALIZED value on stack")
          val moreThanOne = frame.stack.tail.contains(ref)
          assert(!moreThanOne, "More than one UNINITIALIZED value on stack")
          val (ref2, prev2, frame2) = frame.pop()
          frame = frame2.push(V_TYPE(false), prev2)
        }
        else if (!shouldLift && hasVArgs) {
          // Passing V to constructors of classes that we don't lift (e.g. String)
          // There could be ONE or NO duplicate reference on the operand stack.
          // If there is one, we ensure that it is actually duplicated and set the tag to wrap it into V.
          // If there is no, we do nothing.
          val hasDupRef: Boolean = frame.stack.headOption.exists(_._1 == ref)
          if (hasDupRef) {
            env.setTag(this, env.TAG_WRAP_DUPLICATE)
            val moreThanOne = frame.stack.tail.contains(ref)
            assert(!moreThanOne, "More than one duplicated values on stack")
            val (ref2, prev2, frame2) = frame.pop()
            frame = frame2.push(V_TYPE(false), prev2)
          }
        }
      }
    }

    // arguments
    if (hasVArgs || hasArrayArgs) env.setTag(this, env.TAG_HAS_VARG)
    if (hasVArgs || hasArrayArgs || shouldLift || env.shouldLiftInstr(this)) {
      // ensure that all arguments are V
      for (ele <- argList if !ele._1.isInstanceOf[V_TYPE]) return (s, ele._2)
    }

    // return value
    if (Type.getReturnType(desc) != Type.VOID_TYPE) {
      val retV: VBCType = if (MethodDesc(desc).getReturnType.get.is64Bit) V_TYPE(true) else V_TYPE(false)
      if (env.getTag(this, env.TAG_NEED_V))
        frame = frame.push(retV, Set(this))
      else if (env.shouldLiftInstr(this))
        frame = frame.push(retV, Set(this))
      else if (shouldLift || (hasVArgs && !shouldLift))
        frame = frame.push(retV, Set(this))
      else
        frame = frame.push(VBCType(Type.getReturnType(desc)), Set(this))
    }

    // For exception handling, method invocation implies the end of a block
    //    val backtrack = backtraceNonVStackElements(frame)
    //    (frame, backtrack)
    (frame, Set())
  }

  def backtraceNonVStackElements(f: VBCFrame): Set[Instruction] = {
    (Tuple2[VBCType, Set[Instruction]](V_TYPE(false), Set()) /: f.stack) (  // initial V_TYPE(false) is useless
      (a: FrameEntry, b: FrameEntry) => {
        if (!b._1.isInstanceOf[V_TYPE]) (a._1, a._2 ++ b._2)
        else a
      })._2
  }
}

/**
  * INVOKESPECIAL instruction
  *
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKESPECIAL(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESPECIAL, env.liftOwner(owner), name, env.liftDesc(desc), itf)
  }

  /**
    * Generate variability-aware method invocation. For method invocations, we always expect that arguments are V (this
    * limitation could be relaxed later with more optimization). Thus, method descriptors always need to be lifted.
    * Should lift or not depends on the object reference on stack. If the object reference (on which method is going to
    * invoke on) is not a V, we generate INVOKESPECIAL as it is. But if it is a V, we need INVOKEDYNAMIC to invoke
    * methods on all the objects in V.
    *
    * @param mv    MethodWriter
    * @param env   Super environment that contains all the transformation information
    * @param block Current block
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = ???
//  {
//    if (env.shouldLiftInstr(this)) {
//      invokeDynamic(owner, name, desc, itf, mv, env, loadCurrentCtx(_, env, block))
//    }
//    else {
//      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
//      val liftedCall = liftCall(owner, name, desc)
//
//      if (liftedCall.isLifting) loadCurrentCtx(mv, env, block)
//      if (name.contentEquals("<init>") && hasVArgs && !liftedCall.isLifting) {
//        // e.g. passing V<Integer> into String constructor
//        loadCurrentCtx(mv, env, block)
//        invokeInitWithVs(liftedCall, itf, mv, env)
//        return  // avoid redundant One wrapping in the end.
//      }
//      else if (name.contentEquals("<init>") && liftedCall.isLifting && hasVArgs) {
//        pushNulls(mv, desc)
//        mv.visitMethodInsn(INVOKESPECIAL, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
//        //cpwtodo: handle exceptions in <init>
//      }
//      else if (!liftedCall.isLifting && hasVArgs) {
//        loadCurrentCtx(mv, env, block)
//        invokeOnNonV(owner, name, desc, itf, mv, env, block)
//      }
//      else {
//        interceptCalls(liftedCall, mv, env.getVarIdx(env.getVBlockVar(block)), env) {
//          mv.visitMethodInsn(INVOKESPECIAL, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
//        }
////        mv.visitMethodInsn(INVOKESPECIAL, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
//        if (shouldTransformReturnType(liftedCall)) {
//          boxReturnValue(liftedCall.desc, mv)
//        }
//        //cpwtodo: for now, ignore the exceptions on stack
//        if (!name.contentEquals("<init>") && liftedCall.isLifting && desc.isReturnVoid) mv.visitInsn(POP)
//      }
//
//      if (env.getTag(this, env.TAG_WRAP_DUPLICATE) || env.getTag(this, env.TAG_NEED_V))
//        callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
//    }
//  }
//
//  def invokeInitWithVs(liftedCall: LiftedCall, itf: Boolean, mv: MethodVisitor, env: VMethodEnv): Unit = {
//    val args = liftedCall.desc.getArgs.map(_.castInt)
//    val nArgs = args.length
//    val shortClsName = liftedCall.owner.name.split("/").last
//    val helperName = "helper$" + shortClsName + "$init$" + env.clazz.lambdaMethods.size
//    val helperDesc: String = "(" + vclasstype * nArgs + fexprclasstype + s")$vclasstype"
//    val lambdaName = shortClsName + "$init$"
//    val helper = (cv: ClassVisitor) => {
//      val m: MethodVisitor = cv.visitMethod( ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC, helperName, helperDesc, null, Array[String]() )
//      m.visitCode()
//
//      m.visitInsn(ACONST_NULL)  // fake invoke object
//      callVCreateOne(m, loadCtx = (mv) => mv.visitVarInsn(ALOAD, nArgs))
//
//      args.indices foreach { (i) => m.visitVarInsn(ALOAD, i) } // arguments
//      InvokeDynamicUtils.invoke(
//        VCall.sflatMap,
//        m,
//        env,
//        loadCtx = (m) => m.visitVarInsn(ALOAD, nArgs),
//        lambdaName,
//        desc = TypeDesc.getObject + args.map(_.toObject).mkString("(", "", ")") + vclasstype,
//        nExplodeArgs = nArgs,
//        expandArgArray = true
//      ) {
//        (mm: MethodVisitor) => {
//          mm.visitTypeInsn(NEW, liftedCall.owner)
//          mm.visitInsn(DUP)
//          1 until nArgs foreach { i => loadVar(i, liftedCall.desc, i - 1, mm) } // first nArgs - 1 arguments
//          loadVar(nArgs + 1, liftedCall.desc, nArgs - 1 , mm) // last argument
//          mm.visitMethodInsn(INVOKESPECIAL, liftedCall.owner, "<init>", liftedCall.desc, itf)
//          callVCreateOne(mm, mmm => mmm.visitVarInsn(ALOAD, nArgs))
//          mm.visitInsn(ARETURN)
//        }
//      }
//      m.visitInsn(ARETURN)
//      m.visitMaxs(10, 10)
//      m.visitEnd()
//    }
//    env.clazz.lambdaMethods += (helperName -> helper)
//    mv.visitMethodInsn(INVOKESTATIC, Owner(env.clazz.name), MethodName(helperName), MethodDesc(helperDesc), false)
//    // pop useless uninitialized references
//    mv.visitInsn(SWAP); mv.visitInsn(POP); mv.visitInsn(SWAP); mv.visitInsn(POP)
//  }
//
//  def pushNulls(mv: MethodVisitor, origDesc: MethodDesc): Unit = {
//    val args = origDesc.getArgs
//    args.indices foreach {i =>
//      if (args(i).isPrimitive) {
//        args(i) match {
//          case TypeDesc("J") => mv.visitInsn(LCONST_0)
//          case TypeDesc("D") => mv.visitInsn(DCONST_0)
//          case TypeDesc("F") => mv.visitInsn(FCONST_0)
//          case _ => mv.visitInsn(ICONST_0)
//        }
//      }
//      else
//        mv.visitInsn(ACONST_NULL)
//    }
//  }

  /**
    * Used to identify the start of init method
    *
    * @see [[Rewrite.rewrite()]]
    */
  override def isINVOKESPECIAL_OBJECT_INIT: Boolean =
  owner.contentEquals("java/lang/Object") && name.contentEquals("<init>") && desc.contentEquals("()V") && !itf

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
    updateStack(s, env, owner, name, desc)
}


/**
  * INVOKEVIRTUAL instruction
  *
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKEVIRTUAL(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKEVIRTUAL, env.liftOwner(owner), name, env.liftDesc(desc), itf)
  }


  /**
    * Generate variability-aware method invocation.
    *
    * Should lift or not depends on the object reference on stack. If the object reference (on which method is going to
    * invoke on) is not a V, we generate INVOKEVIRTUAL as it is. But if it is a V, we need INVOKEDYNAMIC to invoke
    * methods on all the objects in V.
    *
    * @note Although this looks very similar to the implementation of INVOKESPECIAL, one significant difference is that
    *       INVOKESPECIAL needs special hacking for object initialization. For INVOKESPECIAL, it is possible that we
    *       need to wrap the previously duplicated uninitialized object reference into a V, but it is never the case
    *       for INVOKEVIRTUAL.
    * @param mv    MethodWriter
    * @param env   Super environment that contains all the transformation information
    * @param block Current block
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = ???
//  {
//
//    if (env.shouldLiftInstr(this)) {
//      invokeDynamic(owner, name, desc, itf, mv, env, loadCurrentCtx(_, env, block))
//    }
//    else {
//      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
//      val liftedCall = liftCall(owner, name, desc)
//
//      if (!liftedCall.isLifting && hasVArgs) {
//        loadCurrentCtx(mv, env, block)
//        invokeOnNonV(owner, name, desc, itf, mv, env, block)
//      }
//      else {
//        if (liftedCall.isLifting) loadCurrentCtx(mv, env, block)
//        interceptCalls(liftedCall, mv, env.getVarIdx(env.getVBlockVar(block)), env) {
//          mv.visitMethodInsn(INVOKEVIRTUAL, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
//        }
//        if (env.getTag(this, env.TAG_NEED_V)) {
//          if (shouldTransformReturnType(liftedCall)) {
//            toVArray(liftedCall, mv, env.getBlockVarVIdx(block))
//            boxReturnValue(liftedCall.desc, mv)
//          }
//          callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
//        }
//        // cpwtodo: for now, just pop the returned exceptions.
//        if (liftedCall.isLifting && desc.isReturnVoid) mv.visitInsn(POP)
//      }
//    }
//  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
    updateStack(s, env, owner, name, desc)
}


/**
  * INVOKESTATIC
  *
  * @param owner
  * @param name
  * @param desc
  * @param itf
  */
case class InstrINVOKESTATIC(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
/*CHK
    val replaced = replaceCall(owner, name, desc, isVE = false)
    mv.visitMethodInsn(INVOKESTATIC, replaced.owner, replaced.name, replaced.desc, itf)
*/
    mv.visitMethodInsn(INVOKESTATIC, env.liftOwner(owner), name, env.liftDesc(desc), itf)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = ???
//  {
//    val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
//    val liftedCall = liftCall(owner, name, desc)
//
//    if (!liftedCall.isLifting && hasVArgs) {
//      explodeVArgs(liftedCall, mv, env, block)
//    } else {
//      if (liftedCall.isLifting) loadCurrentCtx(mv, env, block)
//      mv.visitMethodInsn(INVOKESTATIC, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
//      toVArray(liftedCall, mv, env.getVarIdx(env.getVBlockVar(block)))
//      if (env.getTag(this, env.TAG_NEED_V)) {
//        boxReturnValue(liftedCall.desc, mv)
//        callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
//      }
//      //cpwtodo: for now, ignore exceptions on stack
//      if (liftedCall.isLifting && desc.isReturnVoid) mv.visitInsn(POP)
//    }
//  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame =
    updateStack(s, env, owner, name, desc)

//  def explodeVArgs(liftedCall: LiftedCall, mv: MethodVisitor, env: VMethodEnv, block: Block) = {
//    val vCall = if (liftedCall.desc.isReturnVoid) VCall.sforeach else VCall.sflatMap
//    val lambdaName = "helper$invokestaticWithVs$" + env.clazz.lambdaMethods.size
//    val args = liftedCall.desc.getArgs.map(_.castInt).map(_.toObject)
//    val invokeDesc = args.head.desc + s"(${args.tail.map(_.desc).mkString("")})" + liftedCall.desc.getReturnType.map(_.desc).getOrElse("V")
//    InvokeDynamicUtils.invokeWithCacheClear(
//      vCall,
//      mv,
//      env,
//      loadCurrentCtx(_, env, block),
//      lambdaName,
//      invokeDesc,
//      nExplodeArgs = args.length - 1,
//      expandArgArray = true
//    ) {
//      (m: MethodVisitor) => {
//        0 to args.length - 2 foreach { i => loadVar(i, liftedCall.desc, i, m) } // first args.size - 1 arguments
//        loadVar(args.size, liftedCall.desc, args.size - 1, m) // last argument
//        m.visitMethodInsn(INVOKESTATIC, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
//        boxReturnValue(liftedCall.desc, m)
//        toVArray(liftedCall, m, args.size - 1)
//        if (liftedCall.desc.isReturnVoid) {
//          m.visitInsn(RETURN)
//        }
//        else {
//          callVCreateOne(m, mm => mm.visitVarInsn(ALOAD, args.size - 1))
//          m.visitInsn(ARETURN)
//        }
//      }
//    }
//  }
}

case class InstrINVOKEINTERFACE(owner: Owner, name: MethodName, desc: MethodDesc, itf: Boolean) extends MethodInstruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKEINTERFACE, env.liftOwner(owner), name, env.liftDesc(desc), itf)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = updateStack(s, env, owner, name, desc)

  /**
    * Should be the same as INVOKEVIRTUAL
    */
  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = ???
//  {
//    if (env.shouldLiftInstr(this)) {
//      invokeDynamic(owner, name, desc, itf, mv, env, loadCurrentCtx(_, env, block))
//    }
//    else {
//      val hasVArgs = env.getTag(this, env.TAG_HAS_VARG)
//      val liftedCall = liftCall(owner, name, desc)
//
//      if (!liftedCall.isLifting && hasVArgs) {
//        loadCurrentCtx(mv, env, block)
//        invokeOnNonV(owner, name, desc, itf, mv, env, block)
//      }
//      else {
//        if (liftedCall.isLifting) loadCurrentCtx(mv, env, block)
//        mv.visitMethodInsn(INVOKEINTERFACE, liftedCall.owner, liftedCall.name, liftedCall.desc, itf)
//
//        if (env.getTag(this, env.TAG_NEED_V)) callVCreateOne(mv, (m) => loadCurrentCtx(m, env, block))
//      }
//    }
//  }
}

case class InstrINVOKEDYNAMIC(name: MethodName, desc: MethodDesc, bsm: Handle, bsmArgs: Array[Object]) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitInvokeDynamicInsn(name, desc, bsm, bsmArgs:_*)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = ???

  override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = ???
}