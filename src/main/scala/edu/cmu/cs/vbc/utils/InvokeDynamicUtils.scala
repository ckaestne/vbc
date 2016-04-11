package edu.cmu.cs.vbc.utils

import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.vbytecode.{Block, VMethodEnv}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassVisitor, Handle, MethodVisitor, Type}

/**
  * Utilities for doing invokedynamic on V object. For example, GETFIELD, ANEWARRAY, AASTORE, etc.
  *
  * Support nested invokedynamic. For example, AASTORE is usually invoked on a V array ref, with V index and
  * V value on stack.
  *
  * Stack before execution: ... array(V), index(V), value(V)
  * Stack after execution: ...
  *
  * To do invokedynamic on V array reference, the desc parameter for [[InvokeDynamicUtils.invoke()]] method should be:
  *
  * [edu/cmu/cs/varex/V;(Ljava/lang/Integer;Ledu/cmu/cs/varex/V;)V
  *
  * The type before parenthesis is the type of object that invokedynamic is performed on, the types inside parenthesis are
  * types of the arguments, and finally the type after parenthesis is the return type.
  *
  * Since index is a V, we need a nested invokedynamic. The desc for this nested invokedynamic should be:
  *
  * [Ljava/lang/Integer;(Ledu/cmu/cs/varex/V;[edu/cmu/cs/varex/V;)V
  *
  * Note that this desc is different from the previous one because arguments need to be shifted to meet different call sites.
  * Shifting rule is documented in method [[InvokeDynamicUtils.shiftDesc()]].
  *
  * While calling invoke, lambda methods will be generated to explode arguments if necessary, but users need to provide a final
  * lambda method body. For example, for GETFIELD, the method body includes loading object reference and invoking GETFIELD.
  *
  * @author chupanw
  */
object InvokeDynamicUtils {

  val biFuncType = "Ljava/util/function/BiFunction;"
  val biConsumerType = "Ljava/util/function/BiConsumer;"

  /**
    * Perform the sMap operation
    *
    * @param vCallName    method of V interface to be called
    * @param mv           current method visitor
    * @param env          current method environment
    * @param block        current block
    * @param lambdaName   key word for the purpose of this lambda method, mostly for debugging purpose
    * @param desc         Format: typeA(typeB*)typeC
    *                     typeA describe the V object on which operations are performed
    *                     typeB* describe the arguments passed to lambda function
    *                     typeC is the return type of lambda function
    * @param nExplodeArgs number of arguments to explode, arguments to be exploded should be the
    *                     first $nExplodeArgs values in the argument list
    * @param isExploding  whether we are generating lambda methods to explode arguments
    * @param lambdaOp     lambda method body
    */
  def invoke(vCallName: String, mv: MethodVisitor, env: VMethodEnv, block: Block, lambdaName: String, desc: String, nExplodeArgs: Int = 0, isExploding: Boolean = false)
            (lambdaOp: MethodVisitor => Unit): Unit = {

    //////////////////////////////////////////////////
    // Init
    //////////////////////////////////////////////////
    val (invokeDynamicName, vCallDesc, funType, isReturnVoid) = vCallName match {
      case "smap" => ("apply", s"($biFuncType$fexprclasstype)$vclasstype", biFuncType, false)
      case "sforeach" => ("accept", s"($biConsumerType$fexprclasstype)V", biConsumerType, true)
      case "sflatMap" => ("apply", s"($biFuncType$fexprclasstype)$vclasstype", biFuncType, false)
      case _ => throw new RuntimeException("Unsupported dynamic invoke type: " + vCallName)
    }

    //////////////////////////////////////////////////
    // Call INVOKEDYNAMIC
    //////////////////////////////////////////////////
    val (invokeObjectDesc, argsDesc, returnDesc) = decomposeDesc(desc)

    val argTypes: Array[Type] = Type.getArgumentTypes(s"($argsDesc)")
    val nArg = argTypes.size

    val invokeDynamicType = "(" +
      vclasstype * nExplodeArgs + (for (t <- argTypes.drop(nExplodeArgs)) yield t.toString).mkString("") +
      s")$funType"

    val lambdaDesc = "(" +
      vclasstype * nExplodeArgs + (for (t <- argTypes.drop(nExplodeArgs)) yield t.toString).mkString("") +
      s"$fexprclasstype$invokeObjectDesc" +
      s")$returnDesc"

    val n = env.clazz.lambdaMethods.size
    val lambdaMtdName: String = "lambda$" + lambdaName + "$" + n
    mv.visitInvokeDynamicInsn(
      invokeDynamicName,
      invokeDynamicType,
      new Handle(H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc),
      // Arguments:
      Type.getType("(Ljava/lang/Object;Ljava/lang/Object;)" + (if (isReturnVoid) "V" else "Ljava/lang/Object;")),
      new Handle(H_INVOKESTATIC, env.clazz.name, lambdaMtdName, lambdaDesc),
      Type.getType(s"($fexprclasstype$invokeObjectDesc)$returnDesc")
    )
    if (isExploding) {
      loadCtx(mv, env, block, Some(lambdaDesc))
    }
    else {
      loadCtx(mv, env, block, None)
    }
    mv.visitMethodInsn(INVOKEINTERFACE, vclassname, vCallName, vCallDesc, true)

    //////////////////////////////////////////////////
    // Generate lambda method body
    //////////////////////////////////////////////////
    val lambda =
      if (nExplodeArgs != 0) (cv: ClassVisitor) => {
        val mv: MethodVisitor = cv.visitMethod(
          ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
          lambdaMtdName,
          lambdaDesc,
          lambdaDesc,
          Array[String]() // Empty exception list
        )
        mv.visitCode()
        mv.visitVarInsn(ALOAD, 0) // this is the next V to be exploded

        /* load arguments */
        for (i <- 1 until nArg) mv.visitVarInsn(ALOAD, i)
        mv.visitVarInsn(ALOAD, nArg + 1) // last argument of lambda method, the V that just got exploded

        invoke(vCallName, mv, env, block, "explodeArg", shiftDesc(desc), nExplodeArgs - 1, isExploding = true)(lambdaOp)

        if (isReturnVoid) mv.visitInsn(RETURN) else mv.visitInsn(ARETURN)
        mv.visitMaxs(10, 10)
        mv.visitEnd()
      }
      else (cv: ClassVisitor) => {
        val mv: MethodVisitor = cv.visitMethod(
          ACC_PRIVATE | ACC_STATIC | ACC_SYNTHETIC,
          lambdaMtdName,
          lambdaDesc,
          lambdaDesc,
          Array[String]() // Empty exception list
        )
        mv.visitCode()
        lambdaOp(mv)
        mv.visitMaxs(10, 10)
        mv.visitEnd()
      }
    env.clazz.lambdaMethods += (lambdaMtdName -> lambda)

  }


  /**
    * Extract three different parts from desc
    * @param desc descriptor for invokedynamic
    * @return (invoke object descriptor, argument list descriptor, return type descriptor)
    */
  def decomposeDesc(desc: String): (String, String, String) = {
    val split: Array[String] = desc.split('(')
    assert(split.size == 2, "Description format is wrong")
    val split2 = split(1).split(')')
    assert(split2.size == 2, "Description format is wrong")
    (split(0), split2(0), split2(1))
  }

  /**
    * Shift types in descriptor for generating nested invokedynamic
    *
    * Shifting rules:
    * Ref(Arg1, Arg2, Arg3)Ret ->
    * Arg1(Arg2, Arg3, Ref)Ret ->
    * Arg2(Arg3, Ref, Arg1)Ret ->
    * Arg3(Ref, Arg1, Arg2)Ret
    */
  def shiftDesc(desc: String): String = {
    val (invokeObjDesc, argsDesc, returnDesc) = decomposeDesc(desc)
    val argsType: Array[Type] = Type.getArgumentTypes(s"($argsDesc)")

    val newInvokeObjDesc = argsType(0)
    val newArgsDesc: String = (for (i <- argsType.tail) yield i.toString).mkString("") + invokeObjDesc
    newInvokeObjDesc + "(" + newArgsDesc + ")" + returnDesc
  }

  /**
    * Load current ctx.
    *
    * The way of loading current ctx depends on whether we are inside lambda methods
    */
  def loadCtx(mv: MethodVisitor, env: VMethodEnv, block: Block, argsDesc: Option[String]): Unit = {
    if (argsDesc.isDefined) {
      val argTypes: Array[Type] = Type.getArgumentTypes(argsDesc.get)
      val feIdx = argTypes.indexOf(Type.getType(fexprclasstype))
      assert(feIdx >= 0)
      mv.visitVarInsn(ALOAD, feIdx)
    }
    else {
      loadCurrentCtx(mv, env, block)
    }
  }
}
