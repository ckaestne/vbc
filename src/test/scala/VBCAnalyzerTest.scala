package edu.cmu.cs.vbc

import edu.cmu.cs.vbc.analysis.{INT_TYPE, REF_TYPE}
import edu.cmu.cs.vbc.test.InstrDBGIPrint
import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions.{InstrICONST, InstrIFEQ, InstrRETURN}
import org.objectweb.asm.Opcodes._
import org.scalatest.FunSuite


/**
  * Created by ckaestne on 3/19/2016.
  */
class VBCAnalyzerTest extends FunSuite {

    private def method(blocks: Block*) =
        new VBCClassNode(0, 0, "Test", None, "java/lang/Object", Nil, Nil, List(
            new VBCMethodNode(ACC_PUBLIC, "test", "()V", Some("()V"), Nil,
                CFG(blocks.toList))
        ))


    test("basic setup") {

        assert(!(InstrICONST(0) eq InstrICONST(0)))
        assert(InstrICONST(0) != InstrICONST(0))


        val ins0 = InstrICONST(0)
        val ins3 = InstrICONST(3)
        val ins4 = InstrICONST(4)
        val insif = InstrIFEQ(2)
        val c = method(
            Block(ins0, insif),
            Block(ins3, InstrDBGIPrint()),
            Block(ins4, InstrDBGIPrint(), InstrRETURN())
        )

        val env = new VMethodEnv(c, c.methods.head)

        val frames = env.analyzer.computeBeforeFrames

        assert(frames(ins0).stack.isEmpty)
        assert(frames(ins0).localVar(new Parameter(0))._1 == REF_TYPE())
        assert(frames(ins3).stack.isEmpty)
        assert(frames(ins3).localVar(new Parameter(0))._1 == REF_TYPE())
        assert(frames(insif).stack.size == 1)
        assert(frames(insif).stack.head._1 == INT_TYPE())

        println()

    }

}
