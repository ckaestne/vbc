package edu.cmu.cs.vbc.test


import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.varex.{V, VHelper}
import edu.cmu.cs.vbc.instructions.{CFG, Instruction}
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

    val VA = V.choice(FeatureExprFactory.createDefinedExternal("A"), 1, 0)
    val VB = V.choice(FeatureExprFactory.createDefinedExternal("B"), 1, 0)
}

object TestOutput {
    type TOpt[T] = de.fosd.typechef.conditional.Opt[T]

    var output: List[TOpt[String]] = Nil

    def printI(i: Int): Unit = {
        //        println(i)
        output ::= de.fosd.typechef.conditional.Opt(FeatureExprFactory.True, i.toString)
    }

    def printVI(i: V[_ <: Int]): Unit = {
        //        println(i)
        for ((c, v) <- VHelper.explode(FeatureExprFactory.True, i))
            output ::= de.fosd.typechef.conditional.Opt(c, v.asInstanceOf[Int].toString)
    }
}

case class InstrLoadConfig(config: String) extends Instruction {
    override def toByteCode(mv: MethodVisitor, cfg: CFG): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/Config", config, "()I", false)
    }

    override def toVByteCode(mv: MethodVisitor): Unit =
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/Config", "V" + config, "()Ledu/cmu/cs/varex/V;", false)

}


case class InstrDBGIPrint() extends Instruction {
    override def toByteCode(mv: MethodVisitor, cfg: CFG): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printI", "(I)V", false)
    }

    override def toVByteCode(mv: MethodVisitor): Unit =
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printVI", "(Ledu/cmu/cs/varex/V;)V", false)

}