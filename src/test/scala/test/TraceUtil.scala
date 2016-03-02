package edu.cmu.cs.vbc.test


import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import edu.cmu.cs.varex.{V, VHelper}
import edu.cmu.cs.vbc.vbytecode.instructions.Instruction
import edu.cmu.cs.vbc.vbytecode.{Block, MethodEnv, VMethodEnv}
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

import scala.collection.JavaConversions._

/**
  * Package of specific byte code instructions that trace the
  * execution both in the variational and nonvariational way
  * in order to subsequently compare both executions
  *
  * uses lots of global state that should probably be extended
  * at some point to support multithreading. should be okay for
  * now as simple testing infrastructure
  */
object TestTraceOutput {

    trait TraceElement


    var trace: List[(FeatureExpr, String)] = Nil

    val t = FeatureExprFactory.True

    def trace_s(s: String): Unit = {
        trace ::=(t, s)
    }

    def vtrace_s(ctx: FeatureExpr, s: String): Unit = {
        assert(ctx.isInstanceOf[FeatureExpr], s"ctx ($ctx) not FeatureExpr but " + ctx.getClass)
        trace ::=(ctx, s)
    }

    def trace_int(v: Int, s: String): Unit = {
        trace ::=(t, s + ";" + v)
    }

    def vtrace_int(v: V[Int], ctx: FeatureExpr, s: String): Unit = {
        assert(ctx.isInstanceOf[FeatureExpr], "ctx not FeatureExpr but " + ctx.getClass)
        assert(v.isInstanceOf[V[Int]], "v not V[Int] but " + v.getClass)
        for ((ctx, i) <- VHelper.explode(t, v))
            trace ::=(ctx, s + ";" + i)
    }

    def trace_string(s: String): Unit = {
        trace ::=(t, s)
    }

    def vtrace_string(v: V[String], ctx: FeatureExpr): Unit = {
        assert(ctx.isInstanceOf[FeatureExpr], "ctx not FeatureExpr but " + ctx.getClass)
        //v might be of type String for now, due to incomplete lifting
        if (v.isInstanceOf[String])
            trace ::=(ctx, v.asInstanceOf[String])
        else
            for ((ctx, s) <- VHelper.explode(t, v))
                trace ::=(ctx, s)
    }


}


/**
  * trace a single string output
  */
case class TraceInstr_S(s: String) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitLdcInsn(s)
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestTraceOutput", "trace_s", "(Ljava/lang/String;)V", false)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        loadFExpr(mv, env, env.getBlockVar(block))
        mv.visitLdcInsn(s)
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestTraceOutput", "vtrace_s", "(Lde/fosd/typechef/featureexpr/FeatureExpr;Ljava/lang/String;)V", false)
    }
}

/**
  * trace a print instruction, getting the string value on top of the stack
  */
case class TraceInstr_Print() extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestTraceOutput", "trace_string", "(Ljava/lang/String;)V", false)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        mv.visitInsn(DUP)
        loadFExpr(mv, env, env.getBlockVar(block))
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestTraceOutput", "vtrace_string", "(Ledu/cmu/cs/varex/V;Lde/fosd/typechef/featureexpr/FeatureExpr;)V", false)
    }
}

/**
  * trace a getfield instruction (after the instruction) with the value of the field
  * that is now on top of the stack
  */
case class TraceInstr_GetField(s: String, desc: String) extends Instruction {
    override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        assert(desc == "I" || desc == "Z", "only integer fields supported for now")
        mv.visitInsn(DUP)
        mv.visitLdcInsn(s)
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestTraceOutput", "trace_int", "(ILjava/lang/String;)V", false)
    }

    override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = {
        mv.visitInsn(DUP)
        loadFExpr(mv, env, env.getBlockVar(block))
        mv.visitLdcInsn(s)
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestTraceOutput", "vtrace_int", "(Ledu/cmu/cs/varex/V;Lde/fosd/typechef/featureexpr/FeatureExpr;Ljava/lang/String;)V", false)
    }
}
