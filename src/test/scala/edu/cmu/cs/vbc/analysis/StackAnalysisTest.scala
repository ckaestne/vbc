package edu.cmu.cs.vbc.analysis

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc
import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions._
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrLoadConfig, NoLiftPolicy}
import org.objectweb.asm.Opcodes._
import org.scalatest.FunSuite

/**
  * @author chupanw
  */
class StackAnalysisTest extends FunSuite with DiffMethodTestInfrastructure {

  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)


  private def createMethodNode(blocks: Block*) =
    new VBCMethodNode(ACC_PUBLIC, "test", "()V", Some("()V"), Nil, CFG(blocks.toList))

  test("get successors 1") {
    val mn = createMethodNode(
      Block(InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrICONST(1), vbc.InstrDBGIPrint(), InstrGOTO(3)),
      Block(InstrICONST(2), vbc.InstrDBGIPrint(), InstrGOTO(3)),
      Block(InstrICONST(0), vbc.InstrDBGIPrint(), InstrRETURN())
    )

    val cn = new VBCClassNode(V1_8, ACC_PUBLIC, "Test", None, "java/lang/Object", Nil, Nil, Nil) // dummy VBCClassNode
    val env = new VMethodEnv(cn, mn, NoLiftPolicy)
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(0)) == List(1, 2, 3))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(1)) == List(3))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(2)) == List(3))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(3)) == Nil)
  }

  test("get successors 2") {
    val local = new LocalVar("v", "I")

    val mn = createMethodNode(
      Block(InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrICONST(5), InstrISTORE(local), InstrGOTO(3)),
      Block(InstrICONST(10), InstrISTORE(local), InstrGOTO(3)),
      Block(InstrICONST(1), InstrGOTO(4)),
      Block(InstrICONST(2), InstrICONST(3), InstrILOAD(local), InstrICONST(0), InstrIF_ICMPGE(6)),
      Block(vbc.InstrDBGIPrint(), InstrGOTO(7)),
      Block(InstrPOP(), InstrPOP(), InstrILOAD(local), InstrICONST(1), InstrISUB(), InstrISTORE(local), InstrICONST(-1), vbc.InstrDBGIPrint(), InstrGOTO(4)),
      Block(vbc.InstrDBGIPrint(), vbc.InstrDBGIPrint(), InstrICONST(0), vbc.InstrDBGIPrint(), InstrRETURN())
    )

    val cn = new VBCClassNode(V1_8, ACC_PUBLIC, "Test", None, "java/lang/Object", Nil, Nil, Nil) // dummy VBCClassNode
    val env = new VMethodEnv(cn, mn, NoLiftPolicy)
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(0)) == List(1, 2, 3, 4, 5, 6, 7))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(1)) == List(3, 4, 5, 6, 7))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(2)) == List(3, 4, 5, 6, 7))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(3)) == List(4, 5, 6, 7))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(4)) == List(4, 5, 6, 7))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(5)) == List(7))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(6)) == List(4, 5, 6, 7))
    //    assert(env.getOrderedSuccessorsIndexes(env.getBlock(7)) == Nil)
    //
    //    assert(env.getMergePoint(env.getBlock(1), env.getBlock(2)) == 3)
    //    assert(env.getMergePoint(env.getBlock(5), env.getBlock(6)) == 7)
  }

  test("multiple backtrack instructions at merging point") {

    val mn = createMethodNode(
      Block(InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrICONST(1), InstrICONST(2), InstrGOTO(3)),
      Block(InstrICONST(3), InstrLoadConfig("B"), InstrGOTO(3)),
      Block(InstrIADD(), vbc.InstrDBGIPrint(), InstrRETURN())
    )

    val cn = new VBCClassNode(V1_8, ACC_PUBLIC, "Test", None, "java/lang/Object", Nil, Nil, Nil) // dummy VBCClassNode
    val env = new VMethodEnv(cn, mn, NoLiftPolicy)

    env.framesBefore.foreach(println)
  }

  test("multiple backtrack for local variables") {

    val local = new LocalVar("v", "I")

    val mn = createMethodNode(
      Block(InstrLoadConfig("A"), InstrIFNE(2)),
      Block(InstrICONST(1), InstrISTORE(local), InstrGOTO(3)),
      Block(InstrICONST(3), InstrISTORE(local), InstrGOTO(3)),
      Block(InstrILOAD(local), vbc.InstrDBGIPrint(), InstrRETURN())
    )

    val cn = new VBCClassNode(V1_8, ACC_PUBLIC, "Test", None, "java/lang/Object", Nil, Nil, Nil) // dummy VBCClassNode
    val env = new VMethodEnv(cn, mn, NoLiftPolicy)

    env.framesBefore.foreach(println)
  }
}
