package edu.cmu.cs.vbc.utils

import edu.cmu.cs.vbc.vbytecode._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.signature.SignatureReader
import org.objectweb.asm.{MethodVisitor, Type}


object LiftUtils {
  //    val liftedPackagePrefixes = Set("edu.cmu.cs.vbc.test", "edu.cmu.cs.vbc.prog")
  val vclassname = "edu/cmu/cs/varex/V"
  val fexprclassname = "de/fosd/typechef/featureexpr/FeatureExpr"
  val fexprfactoryClassName = "de/fosd/typechef/featureexpr/FeatureExprFactory"
  val vopsclassname = "edu/cmu/cs/varex/VOps"
  val vclasstype = "L" + vclassname + ";"
  val fexprclasstype = "L" + fexprclassname + ";"
  val ctxParameterOffset = 1

  val lamdaFactoryOwner = "java/lang/invoke/LambdaMetafactory"
  val lamdaFactoryMethod = "metafactory"
  val lamdaFactoryDesc = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"


  //    protected def shouldLift(classname: String) = liftedPackagePrefixes.exists(classname startsWith _)

  def liftParameterType(t: Type): String = TypeDesc(t.getDescriptor).getWrapper

  def liftReturnType(t: Type): String = if (t == Type.VOID_TYPE) "V" else vclasstype

  /**
    * lift each parameter and add a new fexpr parameter at the end for the context
    */
  def liftMethodDescription(desc: String): String = {
    val mtype = Type.getMethodType(desc)
    (mtype.getArgumentTypes.map(liftParameterType) :+ Type.getObjectType(fexprclassname)).mkString("(", "", ")") + liftReturnType(mtype.getReturnType)
  }

  def liftCLINIT(name: String): String = {
    if (name == "<clinit>")
      "______clinit______"
    else
      name
  }

  /**
    * lift each parameter but DO NOT add a new fexpr parameter at the end for the context
    * For example, the <init> method of model classes should not contain
    */
  def liftMtdDescNoFE(desc: String): String = {
    val mtype = Type.getMethodType(desc)
    mtype.getArgumentTypes.map(liftParameterType).mkString("(", "", ")") + liftReturnType(mtype.getReturnType)
  }

  def liftMethodSignature(desc: String, sig: Option[String]): Option[String] = {
    val sigReader = new SignatureReader(sig.getOrElse(replaceLibCls(desc)))
    val sw = new LiftSignatureWriter()
    sigReader.accept(sw)
    val newSig = sw.getSignature()
    if (sig != None || newSig != liftMethodDescription(desc))
      Some(newSig)
    else None
  }

  def pushConstant(mv: MethodVisitor, value: Int): Unit = {
    value match {
      case 0 => mv.visitInsn(ICONST_0)
      case 1 => mv.visitInsn(ICONST_1)
      case 2 => mv.visitInsn(ICONST_2)
      case 3 => mv.visitInsn(ICONST_3)
      case 4 => mv.visitInsn(ICONST_4)
      case 5 => mv.visitInsn(ICONST_5)
      case v if v <= Byte.MaxValue && v >= Byte.MinValue => mv.visitIntInsn(BIPUSH, v)
      //TODO other push operation for larger constants
    }
  }

  def pushConstantFALSE(mv: MethodVisitor) =
    mv.visitMethodInsn(INVOKESTATIC, fexprfactoryClassName, "False", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", false)

  def pushConstantTRUE(mv: MethodVisitor) =
    mv.visitMethodInsn(INVOKESTATIC, fexprfactoryClassName, "True", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", false)

  def callFExprIsSatisfiable(mv: MethodVisitor) =
    mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "isSatisfiable", "()Z", true)

  def callFExprIsContradiction(mv: MethodVisitor) =
    mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "isContradiction", "()Z", true)

  def callFExprOr(mv: MethodVisitor) =
    mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "or", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)

  def callFExprAnd(mv: MethodVisitor) =
    mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "and", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)

  def callFExprNot(mv: MethodVisitor) =
    mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "not", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", true)

  def storeFExpr(mv: MethodVisitor, env: MethodEnv, v: Variable) =
    mv.visitVarInsn(ASTORE, env.getVarIdx(v))

  def loadFExpr(mv: MethodVisitor, env: MethodEnv, v: Variable) =
    mv.visitVarInsn(ALOAD, env.getVarIdx(v))

  def loadCurrentCtx(mv: MethodVisitor, env: VMethodEnv, block: Block) =
    if (env.isMain) pushConstantTRUE(mv) else loadFExpr(mv, env, env.getBlockVar(block))

  def storeV(mv: MethodVisitor, env: MethodEnv, v: Variable) =
    mv.visitVarInsn(ASTORE, env.getVarIdx(v))

  def loadV(mv: MethodVisitor, env: MethodEnv, v: Variable) =
    mv.visitVarInsn(ALOAD, env.getVarIdx(v))

  /**
    * precondition: plain reference on top of stack
    * postcondition: V reference on top of stack
    */
  def callVCreateOne(mv: MethodVisitor, loadCtx: (MethodVisitor) => Unit) = {
    loadCtx(mv)
    mv.visitInsn(SWAP)
    mv.visitMethodInsn(INVOKESTATIC, vclassname, "one", s"(${fexprclasstype}Ljava/lang/Object;)$vclasstype", true)
  }

  /**
    * precondition: feature expression and two V references on top of stack
    * postcondition: V reference on top of stack
    */
  def callVCreateChoice(mv: MethodVisitor) =
    mv.visitMethodInsn(INVOKESTATIC, vclassname, "choice", "(Lde/fosd/typechef/featureexpr/FeatureExpr;Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", true)


  def liftPrimitiveType(desc: String): String =
    liftObjectType(primitiveToObjectType(desc))


  def liftObjectType(s: String) = "Ledu/cmu/cs/varex/V<" + s + ">;"


  def primitiveToObjectType(t: String): String = t match {
    case "Z" => "Ljava/lang/Boolean;"
    case "C" => "Ljava/lang/Character;"
    case "B" => "Ljava/lang/Byte;"
    case "S" => "Ljava/lang/Short;"
    case "I" => "Ljava/lang/Integer;"
    case "F" => "Ljava/lang/Float;"
    case "J" => "Ljava/lang/Long;"
    case "D" => "Ljava/lang/Double;"
    case "O" => "Ljava/lang/Object;"
    case _ => t
  }

  /**
    * Helper function to get the method descriptor
    *
    * @param strs strs.last is the return type, the rest is parameter list
    * @return method descriptor according to ASM library standard
    */
  def genSign(strs: String*): String =
    strs.dropRight(1).mkString("(", "", ")") + strs.last

  def replaceArgsWithObject(desc: String): String =
    "(" + "Ljava/lang/Object;" * Type.getArgumentTypes(desc).length + ")" + Type.getReturnType(desc)

  //////////////////////////////////////////////////
  // Model class related utils
  //////////////////////////////////////////////////

  def vCls(cls: String) = s"edu/cmu/cs/vbc/model/$cls"

  def vClsType(cls: String) = s"Ledu/cmu/cs/vbc/model/$cls;"

  val IntClass = "java/lang/Integer"
  val IntType = "Ljava/lang/Integer;"
  val BooleanClass = "java/lang/Boolean"
  val BooleanType = "Ljava/lang/Boolean;"
  val StringClass = "java/lang/String"
  val StringType = "Ljava/lang/String;"
  val ObjectClass = "java/lang/Object"
  val ObjectType = "Ljava/lang/Object;"

  /**
    * Scan and replace java library classes with model classes
    */
  private def replaceLibCls(desc: String): String = {
    val liftType: Type => String =
      (t: Type) => if (t == Type.VOID_TYPE) t.getDescriptor else LiftingPolicy.liftClassType(TypeDesc(t.toString))
    val mtype = Type.getMethodType(desc)
    mtype.getArgumentTypes.map(liftType).mkString("(", "", ")") + liftType(mtype.getReturnType)
  }
}