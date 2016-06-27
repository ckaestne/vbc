package edu.cmu.cs.vbc.test


import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import edu.cmu.cs.varex.{V, VHelper}
import edu.cmu.cs.vbc.analysis.VBCFrame.UpdatedFrame
import edu.cmu.cs.vbc.analysis.{VBCFrame, V_TYPE}
import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.vbytecode.instructions.Instruction
import edu.cmu.cs.vbc.vbytecode.{Block, MethodEnv, VMethodEnv}
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

import scala.collection.JavaConversions._

object Config {

  var configValues: Map[String, Int] = Map("A" -> 0)

  def A = {
    if (configValues contains "A")
      configValues("A")
    else
      throw new RuntimeException("Configuration value for `A` not set")
  }

  def B = {
    if (configValues contains "B")
      configValues("B")
    else
      throw new RuntimeException("Configuration value for `B` not set")
  }

  def C = {
    if (configValues contains "C")
      configValues("C")
    else
      throw new RuntimeException("Configuration value for `C` not set")
  }

  val VA = V.choice(FeatureExprFactory.createDefinedExternal("A"), 1, 0)
  val VB = V.choice(FeatureExprFactory.createDefinedExternal("B"), 1, 0)
  val VC = V.choice(FeatureExprFactory.createDefinedExternal("C"), 1, 0)
}

object TestOutput {
  type TOpt[T] = de.fosd.typechef.conditional.Opt[T]

  var output: List[TOpt[String]] = Nil

  def printI(i: Int): Unit = {
    //        println(i)
    output ::= de.fosd.typechef.conditional.Opt(FeatureExprFactory.True, i.toString)
  }

  def printVI(i: V[_ <: Int], ctx: FeatureExpr): Unit = {
    //        println(i)
    for ((c, v) <- VHelper.explode(ctx, i))
      output ::= de.fosd.typechef.conditional.Opt(c, v.asInstanceOf[Int].toString)
  }

  def printS(s: String): Unit = {
    output ::= de.fosd.typechef.conditional.Opt(FeatureExprFactory.True, s)
  }

  def printVS(s: V[_ <: String], ctx: FeatureExpr): Unit = {
    for ((c, v) <- VHelper.explode(ctx, s))
      output ::= de.fosd.typechef.conditional.Opt(c, v.toString)
  }

  def printFE(f: FeatureExpr): Unit = {
    println(f)
  }

  def printFE(s: String, f: FeatureExpr): Unit = {
    println(s + ": " + f)
  }
}

case class InstrLoadConfig(config: String) extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/Config", config, "()I", false)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit =
    mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/Config", "V" + config, "()Ledu/cmu/cs/varex/V;", false)

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    env.setLift(this)
    (s.push(V_TYPE(), Set(this)), Set())
  }
}


case class InstrDBGIPrint() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printI", "(I)V", false)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    loadFExpr(mv, env, env.getVBlockVar(block)) //ctx
    mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printVI", "(Ledu/cmu/cs/varex/V;Lde/fosd/typechef/featureexpr/FeatureExpr;)V", false)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    env.setLift(this)
    val (v, prev, newFrame) = s.pop()
    val backtrack =
      if (v != V_TYPE()) prev
      else Set[Instruction]()
    (newFrame, backtrack)
  }
}

case class InstrDBGStrPrint() extends Instruction {
  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
    mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printS", "(Ljava/lang/String;)V", false)
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    loadFExpr(mv, env, env.getVBlockVar(block)) //ctx
    mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printVS", "(Ledu/cmu/cs/varex/V;Lde/fosd/typechef/featureexpr/FeatureExpr;)V", false)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = {
    env.setLift(this)
    val (v, prev, newFrame) = s.pop()
    val backtrack =
      if (v != V_TYPE()) prev
      else Set[Instruction]()
    (newFrame, backtrack)
  }
}

case class InstrDBGCtx(name: String) extends Instruction {

  override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
  }

  override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
    mv.visitLdcInsn(name)
    loadFExpr(mv, env, env.getVBlockVar(block)) //ctx
    mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printFE", "(Ljava/lang/String;Lde/fosd/typechef/featureexpr/FeatureExpr;)V", false)
  }

  override def updateStack(s: VBCFrame, env: VMethodEnv): UpdatedFrame = (s, Set())
}